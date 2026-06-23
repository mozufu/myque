use crate::backend::{self, DispatchOutcome, RunRecord};
use crate::board::{BoardOptions, BoardStyle, render_board_with_options, render_task_list};
use crate::eligibility::{
    DispatchPlan, EligibilityDecision, SkipReason, plan_dispatch_for_tasks, skip_reason_text,
};
use crate::model::{Config, Status, Task};
use crate::store::{
    CreateTaskInput, SectionMode, StoredTask, TaskSectionName, TaskStore, UpdateTaskInput,
};
use anyhow::Result;
use clap::{Parser, Subcommand};
use std::collections::{HashMap, HashSet};
use std::ffi::OsString;
use std::io::Read;
use std::path::PathBuf;

#[derive(Debug, Parser)]
#[command(
    name = "myque",
    version,
    about = "Local Markdown task queue for autonomous agents"
)]
pub struct Cli {
    /// Repository root containing .myque/.
    #[arg(long, global = true, default_value = ".")]
    pub root: PathBuf,

    #[command(subcommand)]
    pub command: Command,
}

#[derive(Debug, Subcommand)]
pub enum Command {
    /// Create .myque/config.toml, .myque/tasks, and .myque/runs.
    Init {
        /// Replace an existing config.toml with defaults.
        #[arg(long)]
        force: bool,
    },
    /// Create a task Markdown file under .myque/tasks.
    New {
        title: String,
        /// Explicit task id. Primarily useful for scripts and tests.
        #[arg(long)]
        id: Option<String>,
        #[arg(long, value_parser = parse_status, default_value = "backlog")]
        status: Status,
        #[arg(long, default_value_t = 2)]
        priority: i64,
        #[arg(long, default_value_t = 1000)]
        order: i64,
        #[arg(long = "label")]
        labels: Vec<String>,
        #[arg(long)]
        agent: Option<String>,
        #[arg(long)]
        backend: Option<String>,
        #[arg(long = "depends-on")]
        depends_on: Vec<String>,
        #[arg(long = "allowed-auto-dispatch", alias = "allow-auto")]
        allowed_auto_dispatch: bool,
        #[arg(long)]
        max_attempts: Option<u32>,
    },
    /// List tasks compactly.
    List {
        #[arg(long, value_parser = parse_status)]
        status: Option<Status>,
        #[arg(long = "label")]
        labels: Vec<String>,
        #[arg(long)]
        agent: Option<String>,
    },
    /// Print one task's metadata and Markdown body.
    Show { task_id: String },
    /// Render a Kanban board from task metadata.
    Board {
        /// Render one status group per section instead of side-by-side columns.
        #[arg(long)]
        grouped: bool,
    },
    /// Move a task to a new lifecycle status.
    Move {
        task_id: String,
        #[arg(value_parser = parse_status)]
        status: Status,
    },
    /// Patch task frontmatter while preserving Markdown body.
    Edit {
        task_id: String,
        #[arg(long)]
        title: Option<String>,
        #[arg(long)]
        priority: Option<i64>,
        #[arg(long)]
        order: Option<i64>,
        #[arg(long)]
        agent: Option<String>,
        #[arg(long)]
        backend: Option<String>,
        #[arg(long)]
        max_attempts: Option<u32>,
        #[arg(long = "allow-auto")]
        allow_auto: bool,
        #[arg(long = "no-allow-auto")]
        no_allow_auto: bool,
    },
    /// Add or remove labels idempotently.
    Label {
        task_id: String,
        #[arg(long = "add")]
        add: Vec<String>,
        #[arg(long = "remove")]
        remove: Vec<String>,
    },
    /// Add or remove dependencies idempotently.
    Deps {
        task_id: String,
        #[arg(long = "add")]
        add: Vec<String>,
        #[arg(long = "remove")]
        remove: Vec<String>,
    },
    /// Replace or append one Markdown section.
    Section {
        task_id: String,
        section: TaskSectionName,
        #[arg(allow_hyphen_values = true)]
        text: Option<String>,
        #[arg(long)]
        append: bool,
        #[arg(long)]
        stdin: bool,
    },
    /// Mark a task failed with a reason.
    Fail {
        task_id: String,
        #[arg(long)]
        reason: String,
    },
    /// Mark a task complete for review, or done when policy allows it.
    Complete {
        task_id: String,
        #[arg(long)]
        done: bool,
    },
    /// Validate all task files and config.
    Validate,
    /// Print dispatch eligibility without starting a backend.
    Ready {
        /// Emit stable machine-readable task JSON for runner orchestration.
        #[arg(long)]
        json: bool,
    },
    /// Dispatch eligible tasks through configured backends.
    Dispatch {
        /// Print the selected plan and skip reasons without mutating files.
        #[arg(long)]
        dry_run: bool,
        /// Dispatch only this eligible task.
        #[arg(long)]
        task: Option<String>,
    },
}

pub fn run() -> Result<()> {
    let output = run_with_args(std::env::args_os())?;
    print!("{output}");
    Ok(())
}

pub fn run_with_args<I, T>(args: I) -> Result<String>
where
    I: IntoIterator<Item = T>,
    T: Into<OsString> + Clone,
{
    let cli = Cli::try_parse_from(args)?;
    execute(cli)
}

pub fn execute(cli: Cli) -> Result<String> {
    let store = TaskStore::new(cli.root);
    match cli.command {
        Command::Init { force } => {
            store.init(force)?;
            Ok(format!("Initialized {}\n", store.base_dir().display()))
        }
        Command::New {
            title,
            id,
            status,
            priority,
            order,
            labels,
            agent,
            backend,
            depends_on,
            allowed_auto_dispatch,
            max_attempts,
        } => {
            let config = store.load_config()?;
            let mut input = CreateTaskInput::new(title);
            input.id = id;
            input.status = status;
            input.priority = priority;
            input.order = order;
            input.labels = labels;
            input.agent = agent.unwrap_or_else(|| "coder".to_owned());
            input.backend = backend.unwrap_or_else(|| config.default_backend.clone());
            input.depends_on = depends_on;
            input.allowed_auto_dispatch = allowed_auto_dispatch;
            input.max_attempts = max_attempts.unwrap_or(config.policy.max_attempts_default);
            let stored = store.create_task(input)?;
            Ok(format!(
                "Created {} at {}\n",
                stored.task.id,
                stored.path.display()
            ))
        }
        Command::List {
            status,
            labels,
            agent,
        } => {
            let tasks = store.load_tasks()?;
            let filtered = filtered_tasks(&tasks, status.as_ref(), &labels, agent.as_deref());
            if filtered.is_empty() {
                Ok("No tasks.\n".to_owned())
            } else {
                Ok(render_task_list(filtered))
            }
        }
        Command::Show { task_id } => {
            let stored = store.get_task(&task_id)?;
            Ok(render_task(&stored))
        }
        Command::Board { grouped } => {
            let tasks = store.load_tasks()?;
            let options = BoardOptions {
                style: if grouped {
                    BoardStyle::Grouped
                } else {
                    BoardStyle::Columns
                },
            };
            Ok(render_board_with_options(
                tasks.iter().map(|stored| &stored.task),
                &options,
            ))
        }
        Command::Move { task_id, status } => {
            let stored = store.update_status(&task_id, status)?;
            Ok(format!(
                "Moved {} to {}\n",
                stored.task.id, stored.task.status
            ))
        }
        Command::Edit {
            task_id,
            title,
            priority,
            order,
            agent,
            backend,
            max_attempts,
            allow_auto,
            no_allow_auto,
        } => {
            let allowed_auto_dispatch = match (allow_auto, no_allow_auto) {
                (true, false) => Some(true),
                (false, true) => Some(false),
                _ => None,
            };
            let input = UpdateTaskInput {
                title,
                priority,
                order,
                agent,
                backend,
                max_attempts,
                allowed_auto_dispatch,
            };
            let stored = store.update_task(&task_id, input)?;
            Ok(format!("Updated {}\n", stored.task.id))
        }
        Command::Label {
            task_id,
            add,
            remove,
        } => {
            let stored = store.update_labels(&task_id, &add, &remove)?;
            Ok(format!(
                "Updated labels for {}: {}\n",
                stored.task.id,
                join_or_dash(&stored.task.labels)
            ))
        }
        Command::Deps {
            task_id,
            add,
            remove,
        } => {
            let stored = store.update_dependencies(&task_id, &add, &remove)?;
            Ok(format!(
                "Updated dependencies for {}: {}\n",
                stored.task.id,
                join_or_dash(&stored.task.depends_on)
            ))
        }
        Command::Section {
            task_id,
            section,
            text,
            append,
            stdin,
        } => {
            let mut input = text.unwrap_or_default();
            if stdin {
                std::io::stdin().read_to_string(&mut input)?;
            }
            let mode = if append {
                SectionMode::Append
            } else {
                SectionMode::Replace
            };
            let stored = store.update_section(&task_id, section, &input, mode)?;
            Ok(format!(
                "Updated {} for {}\n",
                section.title(),
                stored.task.id
            ))
        }
        Command::Fail { task_id, reason } => {
            let stored = store.fail_task(&task_id, reason)?;
            Ok(format!("Failed {}\n", stored.task.id))
        }
        Command::Complete { task_id, done } => {
            let config = store.load_config()?;
            let stored = store.complete_task(&task_id, done, &config)?;
            Ok(format!(
                "Completed {} to {}\n",
                stored.task.id, stored.task.status
            ))
        }
        Command::Validate => {
            let config = store.load_config()?;
            let errors = store.validate(&config)?;
            Ok(render_validation(&errors))
        }
        Command::Ready { json } => {
            let config = store.load_config()?;
            let tasks = store.load_tasks()?;
            let plan = plan_dispatch_for_tasks(&tasks, &config, true);
            if json {
                render_ready_json(&tasks, &config, &plan)
            } else {
                Ok(render_ready_report(&tasks, &config, &plan))
            }
        }
        Command::Dispatch { dry_run, task } => {
            let config = store.load_config()?;
            let tasks = store.load_tasks()?;
            let outcome = if let Some(task_id) = task {
                backend::dispatch_task(&store, &config, &task_id, dry_run)?
            } else if dry_run {
                backend::dispatch(&store, &config, true)?
            } else {
                backend::dispatch(&store, &config, false)?
            };
            if dry_run {
                Ok(render_dry_run_report(&tasks, &config, &outcome.plan))
            } else {
                Ok(render_dispatch_outcome(&tasks, &config, &outcome))
            }
        }
    }
}

fn parse_status(value: &str) -> std::result::Result<Status, String> {
    value.parse()
}

fn filtered_tasks<'a>(
    tasks: &'a [StoredTask],
    status: Option<&Status>,
    labels: &[String],
    agent: Option<&str>,
) -> Vec<&'a Task> {
    tasks
        .iter()
        .map(|stored| &stored.task)
        .filter(|task| status.is_none_or(|status| &task.status == status))
        .filter(|task| {
            labels
                .iter()
                .all(|label| task.labels.iter().any(|task_label| task_label == label))
        })
        .filter(|task| agent.is_none_or(|agent| task.agent == agent))
        .collect()
}

fn render_task(stored: &StoredTask) -> String {
    let task = &stored.task;
    let mut out = String::new();
    out.push_str(&format!("id: {}\n", task.id));
    out.push_str(&format!("title: {}\n", task.title));
    out.push_str(&format!("status: {}\n", task.status));
    out.push_str(&format!("priority: {}\n", task.priority));
    out.push_str(&format!("order: {}\n", task.order));
    out.push_str(&format!("labels: {}\n", join_or_dash(&task.labels)));
    out.push_str(&format!("agent: {}\n", task.agent));
    out.push_str(&format!("backend: {}\n", task.backend));
    out.push_str(&format!("depends_on: {}\n", join_or_dash(&task.depends_on)));
    out.push_str(&format!(
        "allowed_auto_dispatch: {}\n",
        task.allowed_auto_dispatch
    ));
    out.push_str(&format!(
        "attempts: {}/{}\n",
        task.attempts, task.max_attempts
    ));
    out.push_str(&format!("created_at: {}\n", task.created_at));
    out.push_str(&format!("updated_at: {}\n", task.updated_at));
    if let Some(last_run_id) = &task.last_run_id {
        out.push_str(&format!("last_run_id: {last_run_id}\n"));
    }
    if let Some(assigned_at) = &task.assigned_at {
        out.push_str(&format!("assigned_at: {assigned_at}\n"));
    }
    out.push_str(&format!("path: {}\n\n", stored.path.display()));
    out.push_str(&stored.body);
    if !out.ends_with('\n') {
        out.push('\n');
    }
    out
}

fn render_validation(errors: &[crate::validation::ValidationError]) -> String {
    if errors.is_empty() {
        return "OK\n".to_owned();
    }
    let mut out = format!(
        "Validation failed ({} issue{}):\n",
        errors.len(),
        if errors.len() == 1 { "" } else { "s" }
    );
    for error in errors {
        out.push_str(&format!("  {error}\n"));
    }
    out
}

#[derive(serde::Serialize)]
struct ReadyTaskJson<'a> {
    id: &'a str,
    title: &'a str,
    status: &'a str,
    priority: i64,
    order: i64,
    labels: &'a [String],
    agent: &'a str,
    backend: &'a str,
    path: String,
    allowed_auto_dispatch: bool,
    attempts: u32,
    max_attempts: u32,
}

fn render_ready_json(tasks: &[StoredTask], config: &Config, plan: &DispatchPlan) -> Result<String> {
    let task_map: HashMap<&str, &StoredTask> = tasks
        .iter()
        .map(|stored| (stored.task.id.as_str(), stored))
        .collect();
    let ready = plan
        .eligible()
        .filter_map(|decision| task_map.get(decision.task_id.as_str()).copied())
        .map(|stored| ReadyTaskJson {
            id: &stored.task.id,
            title: &stored.task.title,
            status: stored.task.status.as_str(),
            priority: stored.task.priority,
            order: stored.task.order,
            labels: &stored.task.labels,
            agent: &stored.task.agent,
            backend: effective_backend(&stored.task, config),
            path: stored.path.display().to_string(),
            allowed_auto_dispatch: stored.task.allowed_auto_dispatch,
            attempts: stored.task.attempts,
            max_attempts: stored.task.max_attempts,
        })
        .collect::<Vec<_>>();
    Ok(format!("{}\n", serde_json::to_string_pretty(&ready)?))
}

fn render_ready_report(tasks: &[StoredTask], config: &Config, plan: &DispatchPlan) -> String {
    let task_map = task_map(tasks);
    let mut out = String::new();
    out.push_str("Eligible:\n");
    let eligible: Vec<&EligibilityDecision> = plan.eligible().collect();
    if eligible.is_empty() {
        out.push_str("  none\n");
    } else {
        for decision in eligible {
            push_task_line(&mut out, decision.task_id.as_str(), &task_map, config, "");
        }
    }
    push_skipped(&mut out, plan, &task_map);
    out
}

fn render_dry_run_report(tasks: &[StoredTask], config: &Config, plan: &DispatchPlan) -> String {
    let task_map = task_map(tasks);
    let selected: HashSet<&str> = plan.selected.iter().map(String::as_str).collect();
    let mut out = String::new();
    out.push_str("Dry run: no files changed.\n");
    out.push_str(&format!(
        "Available slots: {} (running: {})\n",
        plan.available_slots, plan.running
    ));
    out.push_str("Selected:\n");
    if plan.selected.is_empty() {
        out.push_str("  none\n");
    } else {
        for task_id in &plan.selected {
            push_task_line(&mut out, task_id, &task_map, config, "");
        }
    }

    let waiting: Vec<&EligibilityDecision> = plan
        .eligible()
        .filter(|decision| !selected.contains(decision.task_id.as_str()))
        .collect();
    if !waiting.is_empty() {
        out.push_str("Eligible but not selected:\n");
        for decision in waiting {
            push_task_line(&mut out, decision.task_id.as_str(), &task_map, config, "");
        }
    }

    push_skipped(&mut out, plan, &task_map);
    out
}

fn render_dispatch_outcome(
    tasks: &[StoredTask],
    config: &Config,
    outcome: &DispatchOutcome,
) -> String {
    let task_map = task_map(tasks);
    let mut out = String::new();
    out.push_str("Started:\n");
    if outcome.started.is_empty() {
        out.push_str("  none\n");
    } else {
        for record in &outcome.started {
            push_run_record(&mut out, record, &task_map);
        }
    }

    out.push_str("Rejected:\n");
    if outcome.rejected.is_empty() {
        out.push_str("  none\n");
    } else {
        for (task_id, reason) in &outcome.rejected {
            push_task_line(
                &mut out,
                task_id,
                &task_map,
                config,
                &format!(" — {}", skip_reason_text(reason)),
            );
        }
    }

    push_skipped(&mut out, &outcome.plan, &task_map);
    out
}

fn push_skipped(out: &mut String, plan: &DispatchPlan, task_map: &HashMap<&str, &Task>) {
    out.push_str("Skipped:\n");
    let skipped: Vec<&EligibilityDecision> = plan.skipped().collect();
    if skipped.is_empty() {
        out.push_str("  none\n");
        return;
    }
    for decision in skipped {
        let reason = join_reasons(&decision.reasons);
        match task_map.get(decision.task_id.as_str()) {
            Some(task) => out.push_str(&format!(
                "  {:<26} {:<30} {}\n",
                task.id, task.title, reason
            )),
            None => out.push_str(&format!("  {:<26} {}\n", decision.task_id, reason)),
        }
    }
}

fn push_task_line(
    out: &mut String,
    task_id: &str,
    task_map: &HashMap<&str, &Task>,
    config: &Config,
    suffix: &str,
) {
    match task_map.get(task_id) {
        Some(task) => out.push_str(&format!(
            "  {:<26} {:<30} agent={} backend={}{}\n",
            task.id,
            task.title,
            task.agent,
            effective_backend(task, config),
            suffix
        )),
        None => out.push_str(&format!("  {task_id}{suffix}\n")),
    }
}

fn push_run_record(out: &mut String, record: &RunRecord, task_map: &HashMap<&str, &Task>) {
    let title = task_map
        .get(record.task_id.as_str())
        .map(|task| task.title.as_str())
        .unwrap_or("");
    out.push_str(&format!(
        "  {:<26} {:<30} run={} backend={} status={}\n",
        record.task_id, title, record.id, record.backend, record.status
    ));
}

fn task_map(tasks: &[StoredTask]) -> HashMap<&str, &Task> {
    tasks
        .iter()
        .map(|stored| (stored.task.id.as_str(), &stored.task))
        .collect()
}

fn effective_backend<'a>(task: &'a Task, config: &'a Config) -> &'a str {
    config
        .agents
        .get(&task.agent)
        .map(|agent| agent.backend.as_str())
        .unwrap_or(task.backend.as_str())
}

fn join_reasons(reasons: &[SkipReason]) -> String {
    if reasons.is_empty() {
        return "selected".to_owned();
    }
    reasons
        .iter()
        .map(skip_reason_text)
        .collect::<Vec<String>>()
        .join("; ")
}

fn join_or_dash(values: &[String]) -> String {
    if values.is_empty() {
        "-".to_owned()
    } else {
        values.join(", ")
    }
}
