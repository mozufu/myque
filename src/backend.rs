use crate::eligibility::{plan_dispatch, DispatchPlan, SkipReason};
use crate::model::{AgentConfig, Config, Status, Task};
use crate::store::{atomic_write, now_rfc3339, StoreError, StoredTask, TaskStore};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::PathBuf;
use std::process::Command;

#[derive(Debug, Clone)]
pub struct BackendDecision {
    pub allowed: bool,
    pub reason: Option<String>,
}

impl BackendDecision {
    pub fn allowed() -> Self {
        Self {
            allowed: true,
            reason: None,
        }
    }

    pub fn rejected(reason: impl Into<String>) -> Self {
        Self {
            allowed: false,
            reason: Some(reason.into()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct DispatchResult {
    pub run_id: String,
    pub started: bool,
    pub message: String,
    pub ended_at: Option<String>,
    pub exit_code: Option<i32>,
}

#[derive(Debug, Clone)]
pub struct RunStatus {
    pub run_id: String,
    pub status: String,
    pub message: Option<String>,
}

pub trait AgentBackend {
    fn name(&self) -> &'static str;
    fn can_run(&self, task: &Task, config: &Config) -> BackendDecision;
    fn dispatch(&self, task: &StoredTask, config: &Config, run_id: String) -> DispatchResult;
    fn status(&self, run_id: &str, config: &Config) -> RunStatus;
    fn cancel(&self, run_id: &str, config: &Config) -> Result<(), BackendError>;
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RunRecord {
    pub id: String,
    pub task_id: String,
    pub backend: String,
    pub agent: String,
    pub status: String,
    pub started_at: String,
    pub ended_at: String,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub exit_code: Option<i32>,
}

#[derive(Debug, Clone)]
pub struct DispatchOutcome {
    pub plan: DispatchPlan,
    pub started: Vec<RunRecord>,
    pub rejected: Vec<(String, SkipReason)>,
}

#[derive(Debug, thiserror::Error)]
pub enum BackendError {
    #[error("store error: {0}")]
    Store(#[from] StoreError),
    #[error("unknown backend: {0}")]
    UnknownBackend(String),
    #[error("missing shell command for agent {0}")]
    MissingShellCommand(String),
    #[error("invalid shell command for agent {agent}: {command}")]
    InvalidShellCommand { agent: String, command: String },
    #[error("failed to run shell backend for task {task_id}: {source}")]
    ShellIo {
        task_id: String,
        #[source]
        source: std::io::Error,
    },
}

pub fn dispatch(
    store: &TaskStore,
    config: &Config,
    dry_run: bool,
) -> Result<DispatchOutcome, BackendError> {
    let mut plan = plan_dispatch(store, config, dry_run)?;
    if dry_run {
        plan.selected.clear();
        return Ok(DispatchOutcome {
            plan,
            started: Vec::new(),
            rejected: Vec::new(),
        });
    }

    let mut started = Vec::new();
    let mut rejected = Vec::new();
    for task_id in plan.selected.clone() {
        let stored = store.get_task(&task_id)?;
        let backend_name = backend_name_for(&stored.task, config).to_owned();
        let backend = backend_for(&backend_name)?;
        let decision = backend.can_run(&stored.task, config);
        if !decision.allowed {
            rejected.push((
                task_id,
                SkipReason::BackendRejected {
                    reason: decision
                        .reason
                        .unwrap_or_else(|| "backend rejected task".to_owned()),
                },
            ));
            continue;
        }

        let run_id = next_run_id(store, &stored.task.id)?;
        let dispatch_result = backend.dispatch(&stored, config, run_id.clone());
        if !dispatch_result.started {
            rejected.push((
                stored.task.id.clone(),
                SkipReason::BackendRejected {
                    reason: dispatch_result.message,
                },
            ));
            continue;
        }

        let started_at = now_rfc3339();
        let mut updated = stored.clone();
        updated.task.attempts = updated.task.attempts.saturating_add(1);
        updated.task.status = Status::Running;
        updated.task.last_run_id = Some(run_id.clone());
        updated.task.assigned_at = Some(started_at.clone());
        updated.task.updated_at = started_at.clone();
        updated.frontmatter.attempts = Some(updated.task.attempts);
        updated.frontmatter.status = Some(Status::Running.to_string());
        updated.frontmatter.last_run_id = Some(run_id.clone());
        updated.frontmatter.assigned_at = Some(started_at.clone());
        updated.frontmatter.updated_at = Some(started_at.clone());

        let record = RunRecord {
            id: run_id,
            task_id: updated.task.id.clone(),
            backend: backend_name,
            agent: updated.task.agent.clone(),
            status: "started".to_owned(),
            started_at,
            ended_at: dispatch_result.ended_at.unwrap_or_default(),
            message: dispatch_result.message,
            exit_code: dispatch_result.exit_code,
        };

        store.write_task(&updated)?;
        write_run_record(store, &record)?;
        started.push(record);
    }

    Ok(DispatchOutcome {
        plan,
        started,
        rejected,
    })
}

pub struct NoopBackend;

impl AgentBackend for NoopBackend {
    fn name(&self) -> &'static str {
        "noop"
    }

    fn can_run(&self, _task: &Task, _config: &Config) -> BackendDecision {
        BackendDecision::allowed()
    }

    fn dispatch(&self, _task: &StoredTask, _config: &Config, run_id: String) -> DispatchResult {
        DispatchResult {
            run_id,
            started: true,
            message: "noop backend accepted task".to_owned(),
            ended_at: None,
            exit_code: None,
        }
    }

    fn status(&self, run_id: &str, _config: &Config) -> RunStatus {
        RunStatus {
            run_id: run_id.to_owned(),
            status: "running".to_owned(),
            message: None,
        }
    }

    fn cancel(&self, _run_id: &str, _config: &Config) -> Result<(), BackendError> {
        Ok(())
    }
}

pub struct ShellBackend;

impl AgentBackend for ShellBackend {
    fn name(&self) -> &'static str {
        "shell"
    }

    fn can_run(&self, task: &Task, config: &Config) -> BackendDecision {
        match agent_config(task, config).and_then(|agent| agent.command.as_deref()) {
            Some(command) if !command.trim().is_empty() => BackendDecision::allowed(),
            _ => BackendDecision::rejected(format!("agent `{}` has no shell command", task.agent)),
        }
    }

    fn dispatch(&self, task: &StoredTask, config: &Config, run_id: String) -> DispatchResult {
        let Some(command_template) =
            agent_config(&task.task, config).and_then(|agent| agent.command.as_deref())
        else {
            return DispatchResult {
                run_id,
                started: false,
                message: format!("agent `{}` has no shell command", task.task.agent),
                ended_at: None,
                exit_code: None,
            };
        };
        let Some(parts) = shlex::split(command_template) else {
            return DispatchResult {
                run_id,
                started: false,
                message: format!("invalid shell command: {command_template}"),
                ended_at: None,
                exit_code: None,
            };
        };
        let Some((program_template, arg_templates)) = parts.split_first() else {
            return DispatchResult {
                run_id,
                started: false,
                message: "empty shell command".to_owned(),
                ended_at: None,
                exit_code: None,
            };
        };
        let program = expand_shell_arg(program_template, task);
        let args: Vec<String> = arg_templates
            .iter()
            .map(|arg| expand_shell_arg(arg, task))
            .collect();

        match Command::new(program).args(args).output() {
            Ok(output) => {
                let exit_code = output.status.code();
                let mut message = if output.status.success() {
                    "shell backend command completed".to_owned()
                } else {
                    "shell backend command failed".to_owned()
                };
                let stderr = String::from_utf8_lossy(&output.stderr);
                if !stderr.trim().is_empty() {
                    message.push_str(": ");
                    message.push_str(stderr.trim());
                }
                DispatchResult {
                    run_id,
                    started: output.status.success(),
                    message,
                    ended_at: Some(now_rfc3339()),
                    exit_code,
                }
            }
            Err(err) => DispatchResult {
                run_id,
                started: false,
                message: err.to_string(),
                ended_at: Some(now_rfc3339()),
                exit_code: None,
            },
        }
    }

    fn status(&self, run_id: &str, _config: &Config) -> RunStatus {
        RunStatus {
            run_id: run_id.to_owned(),
            status: "unknown".to_owned(),
            message: None,
        }
    }

    fn cancel(&self, _run_id: &str, _config: &Config) -> Result<(), BackendError> {
        Ok(())
    }
}

pub fn write_run_record(store: &TaskStore, record: &RunRecord) -> Result<PathBuf, StoreError> {
    let path = store.runs_dir().join(format!("{}.toml", record.id));
    let raw = toml::to_string_pretty(record).expect("run record serializes");
    atomic_write(&path, raw.as_bytes())?;
    Ok(path)
}

pub fn read_run_record(store: &TaskStore, run_id: &str) -> Result<RunRecord, StoreError> {
    let path = store.runs_dir().join(format!("{run_id}.toml"));
    let raw = fs::read_to_string(&path).map_err(|source| StoreError::Io {
        path: path.clone(),
        source,
    })?;
    toml::from_str(&raw).map_err(|source| StoreError::InvalidConfig { path, source })
}

fn backend_for(name: &str) -> Result<Box<dyn AgentBackend>, BackendError> {
    match name {
        "noop" => Ok(Box::new(NoopBackend)),
        "shell" => Ok(Box::new(ShellBackend)),
        other => Err(BackendError::UnknownBackend(other.to_owned())),
    }
}

fn backend_name_for<'a>(task: &'a Task, config: &'a Config) -> &'a str {
    agent_config(task, config)
        .map(|agent| agent.backend.as_str())
        .unwrap_or(task.backend.as_str())
}

fn agent_config<'a>(task: &Task, config: &'a Config) -> Option<&'a AgentConfig> {
    config.agents.get(&task.agent)
}

fn expand_shell_arg(template: &str, task: &StoredTask) -> String {
    template
        .replace("{task_file}", &task.path.display().to_string())
        .replace("{task_id}", &task.task.id)
        .replace("{workspace}", ".")
}

fn next_run_id(store: &TaskStore, task_id: &str) -> Result<String, StoreError> {
    let safe_task = task_id
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
                ch
            } else {
                '-'
            }
        })
        .collect::<String>();
    let stamp = now_rfc3339()
        .chars()
        .filter(|ch| ch.is_ascii_alphanumeric())
        .collect::<String>();
    for suffix in 0..1000 {
        let id = if suffix == 0 {
            format!("run-{stamp}-{safe_task}")
        } else {
            format!("run-{stamp}-{safe_task}-{suffix}")
        };
        let path = store.runs_dir().join(format!("{id}.toml"));
        if !path.exists() {
            return Ok(id);
        }
    }
    Ok(format!("run-{stamp}-{safe_task}-overflow"))
}
