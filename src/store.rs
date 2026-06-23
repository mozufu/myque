use crate::frontmatter::{FrontmatterError, parse_task_file, write_task_file};
use crate::model::{AgentConfig, BackendConfig, Config, Status, Task, TaskFrontmatter};
use crate::validation::{self, ValidationError};
use chrono::Utc;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

pub const DEFAULT_MYQUE_DIR: &str = ".myque";
pub const TASKS_DIR: &str = "tasks";
pub const RUNS_DIR: &str = "runs";
pub const CONFIG_FILE: &str = "config.toml";

#[derive(Debug, Clone)]
pub struct StoredTask {
    pub task: Task,
    pub frontmatter: TaskFrontmatter,
    pub body: String,
    pub path: PathBuf,
}

#[derive(Debug, Clone)]
pub struct CreateTaskInput {
    pub id: Option<String>,
    pub title: String,
    pub status: Status,
    pub priority: i64,
    pub order: i64,
    pub labels: Vec<String>,
    pub agent: String,
    pub backend: String,
    pub depends_on: Vec<String>,
    pub allowed_auto_dispatch: bool,
    pub max_attempts: u32,
    pub body: Option<String>,
}

impl CreateTaskInput {
    pub fn new(title: impl Into<String>) -> Self {
        Self {
            id: None,
            title: title.into(),
            status: Status::Backlog,
            priority: 2,
            order: 1000,
            labels: Vec::new(),
            agent: "coder".to_owned(),
            backend: "noop".to_owned(),
            depends_on: Vec::new(),
            allowed_auto_dispatch: false,
            max_attempts: 2,
            body: None,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct UpdateTaskInput {
    pub title: Option<String>,
    pub priority: Option<i64>,
    pub order: Option<i64>,
    pub agent: Option<String>,
    pub backend: Option<String>,
    pub max_attempts: Option<u32>,
    pub allowed_auto_dispatch: Option<bool>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SectionMode {
    Replace,
    Append,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TaskSectionName {
    Goal,
    Context,
    Constraints,
    Acceptance,
    Files,
    Notes,
}

impl TaskSectionName {
    pub fn title(self) -> &'static str {
        match self {
            Self::Goal => "Goal",
            Self::Context => "Context",
            Self::Constraints => "Constraints",
            Self::Acceptance => "Acceptance",
            Self::Files => "Files",
            Self::Notes => "Notes",
        }
    }
}

impl std::str::FromStr for TaskSectionName {
    type Err = String;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value.to_ascii_lowercase().as_str() {
            "goal" => Ok(Self::Goal),
            "context" => Ok(Self::Context),
            "constraints" => Ok(Self::Constraints),
            "acceptance" => Ok(Self::Acceptance),
            "files" => Ok(Self::Files),
            "notes" => Ok(Self::Notes),
            _ => Err(format!("unknown section `{value}`")),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum SectionUpdateError {
    #[error("missing section text; pass text or --stdin")]
    EmptyInput,
}

#[derive(Debug, Clone)]
pub struct TaskStore {
    root: PathBuf,
}

#[derive(Debug, thiserror::Error)]
pub enum StoreError {
    #[error("I/O error at {path}: {source}")]
    Io {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },
    #[error("invalid config at {path}: {source}")]
    InvalidConfig {
        path: PathBuf,
        #[source]
        source: toml::de::Error,
    },
    #[error("invalid task frontmatter in {path}: {source}")]
    Frontmatter {
        path: PathBuf,
        #[source]
        source: FrontmatterError,
    },
    #[error("validation failed")]
    Validation(Vec<ValidationError>),
    #[error(transparent)]
    SectionUpdate(#[from] SectionUpdateError),
    #[error("task not found: {0}")]
    TaskNotFound(String),
    #[error("task id already exists: {0}")]
    DuplicateTaskId(String),
    #[error("task path has no filename: {0}")]
    MissingFileName(PathBuf),
    #[error("could not create unique path for task id {0}")]
    PathCollision(String),
}

pub type StoreResult<T> = Result<T, StoreError>;

impl Config {
    pub fn load_or_default(root: impl AsRef<Path>) -> StoreResult<Self> {
        let config_path = root.as_ref().join(DEFAULT_MYQUE_DIR).join(CONFIG_FILE);
        if !config_path.exists() {
            return Ok(default_config());
        }
        let raw = fs::read_to_string(&config_path).map_err(|source| StoreError::Io {
            path: config_path.clone(),
            source,
        })?;
        let parsed = toml::from_str(&raw).map_err(|source| StoreError::InvalidConfig {
            path: config_path,
            source,
        })?;
        Ok(with_builtin_defaults(parsed))
    }

    pub fn write_default(root: impl AsRef<Path>, force: bool) -> StoreResult<()> {
        let base = root.as_ref().join(DEFAULT_MYQUE_DIR);
        fs::create_dir_all(&base).map_err(|source| StoreError::Io {
            path: base.clone(),
            source,
        })?;
        let config_path = base.join(CONFIG_FILE);
        if config_path.exists() && !force {
            return Ok(());
        }
        let raw = toml::to_string_pretty(&default_config()).expect("default config serializes");
        atomic_write(&config_path, raw.as_bytes())
    }
}

impl TaskStore {
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self { root: root.into() }
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn base_dir(&self) -> PathBuf {
        self.root.join(DEFAULT_MYQUE_DIR)
    }

    pub fn tasks_dir(&self) -> PathBuf {
        self.base_dir().join(TASKS_DIR)
    }

    pub fn runs_dir(&self) -> PathBuf {
        self.base_dir().join(RUNS_DIR)
    }

    pub fn config_path(&self) -> PathBuf {
        self.base_dir().join(CONFIG_FILE)
    }

    pub fn init(&self, force: bool) -> StoreResult<()> {
        let tasks_dir = self.tasks_dir();
        fs::create_dir_all(&tasks_dir).map_err(|source| StoreError::Io {
            path: tasks_dir,
            source,
        })?;
        let runs_dir = self.runs_dir();
        fs::create_dir_all(&runs_dir).map_err(|source| StoreError::Io {
            path: runs_dir,
            source,
        })?;
        Config::write_default(&self.root, force)
    }

    pub fn load_config(&self) -> StoreResult<Config> {
        Config::load_or_default(&self.root)
    }

    pub fn write_config(&self, config: &Config) -> StoreResult<()> {
        let raw = toml::to_string_pretty(config).expect("config serializes");
        atomic_write(&self.config_path(), raw.as_bytes())
    }

    pub fn load_tasks(&self) -> StoreResult<Vec<StoredTask>> {
        let paths = self.task_paths()?;
        let mut tasks = Vec::with_capacity(paths.len());
        let mut errors = Vec::new();

        for path in paths {
            match self.load_task_file(&path) {
                Ok(task) => tasks.push(task),
                Err(StoreError::Validation(mut validation_errors)) => {
                    errors.append(&mut validation_errors)
                }
                Err(err) => return Err(err),
            }
        }

        let parsed: Vec<Task> = tasks.iter().map(|stored| stored.task.clone()).collect();
        errors.extend(validation::validate_unique_ids(&parsed));
        if errors.is_empty() {
            Ok(tasks)
        } else {
            Err(StoreError::Validation(errors))
        }
    }

    pub fn load_task_map(&self) -> StoreResult<HashMap<String, StoredTask>> {
        let mut map = HashMap::new();
        for stored in self.load_tasks()? {
            map.insert(stored.task.id.clone(), stored);
        }
        Ok(map)
    }

    pub fn get_task(&self, id: &str) -> StoreResult<StoredTask> {
        self.load_tasks()?
            .into_iter()
            .find(|stored| stored.task.id == id)
            .ok_or_else(|| StoreError::TaskNotFound(id.to_owned()))
    }

    pub fn create_task(&self, input: CreateTaskInput) -> StoreResult<StoredTask> {
        let tasks_dir = self.tasks_dir();
        fs::create_dir_all(&tasks_dir).map_err(|source| StoreError::Io {
            path: tasks_dir,
            source,
        })?;

        let now = now_rfc3339();
        let id = match input.id {
            Some(id) => {
                self.ensure_unique_id(&id)?;
                id
            }
            None => self.unique_generated_task_id(&now)?,
        };
        let body = input.body.unwrap_or_else(default_task_body);
        let frontmatter = TaskFrontmatter {
            id: Some(id.clone()),
            title: Some(input.title),
            status: Some(input.status.to_string()),
            priority: Some(input.priority),
            order: Some(input.order),
            labels: Some(input.labels),
            agent: Some(input.agent),
            backend: Some(input.backend),
            depends_on: Some(input.depends_on),
            allowed_auto_dispatch: Some(input.allowed_auto_dispatch),
            attempts: Some(0),
            max_attempts: Some(input.max_attempts),
            created_at: Some(now.clone()),
            updated_at: Some(now),
            milestone: None,
            risk: None,
            review_required: None,
            paths: None,
            last_run_id: None,
            assigned_at: None,
            completed_at: None,
            failure_reason: None,
        };
        self.create_task_from_frontmatter(frontmatter, &body)
    }

    pub fn create_task_from_frontmatter(
        &self,
        frontmatter: TaskFrontmatter,
        body: &str,
    ) -> StoreResult<StoredTask> {
        let tasks_dir = self.tasks_dir();
        fs::create_dir_all(&tasks_dir).map_err(|source| StoreError::Io {
            path: tasks_dir,
            source,
        })?;
        let id = frontmatter.id.clone().unwrap_or_else(|| "task".to_owned());
        self.ensure_unique_id(&id)?;
        let path = self.unique_task_path(&id)?;
        let raw =
            write_task_file(&frontmatter, body).map_err(|source| StoreError::Frontmatter {
                path: path.clone(),
                source,
            })?;
        atomic_write(&path, raw.as_bytes())?;
        self.load_task_file(&path)
    }

    pub fn write_task(&self, stored: &StoredTask) -> StoreResult<()> {
        let raw = write_task_file(&stored.frontmatter, &stored.body).map_err(|source| {
            StoreError::Frontmatter {
                path: stored.path.clone(),
                source,
            }
        })?;
        atomic_write(&stored.path, raw.as_bytes())
    }

    pub fn update_status(&self, id: &str, status: Status) -> StoreResult<StoredTask> {
        let mut stored = self.get_task(id)?;
        stored.task.status = status.clone();
        stored.task.updated_at = now_rfc3339();
        stored.frontmatter.status = Some(status.to_string());
        stored.frontmatter.updated_at = Some(stored.task.updated_at.clone());
        self.write_task(&stored)?;
        Ok(stored)
    }

    pub fn update_task(&self, id: &str, input: UpdateTaskInput) -> StoreResult<StoredTask> {
        let mut stored = self.get_task(id)?;
        if let Some(title) = input.title {
            stored.task.title = title.clone();
            stored.frontmatter.title = Some(title);
        }
        if let Some(priority) = input.priority {
            stored.task.priority = priority;
            stored.frontmatter.priority = Some(priority);
        }
        if let Some(order) = input.order {
            stored.task.order = order;
            stored.frontmatter.order = Some(order);
        }
        if let Some(agent) = input.agent {
            stored.task.agent = agent.clone();
            stored.frontmatter.agent = Some(agent);
        }
        if let Some(backend) = input.backend {
            stored.task.backend = backend.clone();
            stored.frontmatter.backend = Some(backend);
        }
        if let Some(max_attempts) = input.max_attempts {
            stored.task.max_attempts = max_attempts;
            stored.frontmatter.max_attempts = Some(max_attempts);
        }
        if let Some(allowed_auto_dispatch) = input.allowed_auto_dispatch {
            stored.task.allowed_auto_dispatch = allowed_auto_dispatch;
            stored.frontmatter.allowed_auto_dispatch = Some(allowed_auto_dispatch);
        }
        self.write_validated_task(stored)
    }

    pub fn update_labels(
        &self,
        id: &str,
        add: &[String],
        remove: &[String],
    ) -> StoreResult<StoredTask> {
        let mut stored = self.get_task(id)?;
        for label in add {
            if !stored.task.labels.contains(label) {
                stored.task.labels.push(label.clone());
            }
        }
        stored.task.labels.retain(|label| !remove.contains(label));
        stored.frontmatter.labels = Some(stored.task.labels.clone());
        self.write_validated_task(stored)
    }

    pub fn update_dependencies(
        &self,
        id: &str,
        add: &[String],
        remove: &[String],
    ) -> StoreResult<StoredTask> {
        let mut stored = self.get_task(id)?;
        for dependency in add {
            if !stored.task.depends_on.contains(dependency) {
                stored.task.depends_on.push(dependency.clone());
            }
        }
        stored
            .task
            .depends_on
            .retain(|dependency| !remove.contains(dependency));
        stored.frontmatter.depends_on = Some(stored.task.depends_on.clone());
        self.write_validated_dependency_update(stored)
    }

    pub fn update_section(
        &self,
        id: &str,
        section: TaskSectionName,
        text: &str,
        mode: SectionMode,
    ) -> StoreResult<StoredTask> {
        if text.is_empty() {
            return Err(SectionUpdateError::EmptyInput.into());
        }
        let mut stored = self.get_task(id)?;
        stored.body = update_section_body(&stored.body, section, text, mode);
        self.write_validated_task(stored)
    }

    pub fn fail_task(&self, id: &str, reason: String) -> StoreResult<StoredTask> {
        let mut stored = self.get_task(id)?;
        let now = now_rfc3339();
        stored.task.status = Status::Failed;
        stored.task.failure_reason = Some(reason.clone());
        stored.task.completed_at = Some(now.clone());
        stored.task.updated_at = now.clone();
        stored.frontmatter.status = Some(Status::Failed.to_string());
        stored.frontmatter.failure_reason = Some(reason);
        stored.frontmatter.completed_at = Some(now.clone());
        stored.frontmatter.updated_at = Some(now);
        self.write_validated_stored_task(&stored)?;
        Ok(stored)
    }

    pub fn complete_task(&self, id: &str, done: bool, config: &Config) -> StoreResult<StoredTask> {
        let status = if done {
            if !config.policy.agents_may_mark_done {
                return Err(StoreError::Validation(vec![ValidationError {
                    code: validation::ErrorCode::TaskInvalidStatus,
                    context: Some(id.to_owned()),
                    message: "agents may not mark tasks done by policy".to_owned(),
                }]));
            }
            Status::Done
        } else {
            Status::Review
        };
        let mut stored = self.get_task(id)?;
        let now = now_rfc3339();
        stored.task.status = status.clone();
        stored.task.completed_at = Some(now.clone());
        stored.task.updated_at = now.clone();
        stored.frontmatter.status = Some(status.to_string());
        stored.frontmatter.completed_at = Some(now.clone());
        stored.frontmatter.updated_at = Some(now);
        self.write_validated_stored_task(&stored)?;
        Ok(stored)
    }

    fn write_validated_task(&self, mut stored: StoredTask) -> StoreResult<StoredTask> {
        let now = now_rfc3339();
        stored.task.updated_at = now.clone();
        stored.frontmatter.updated_at = Some(now);
        self.write_validated_stored_task(&stored)?;
        self.load_task_file(&stored.path)
    }

    fn write_validated_dependency_update(&self, mut stored: StoredTask) -> StoreResult<StoredTask> {
        let now = now_rfc3339();
        stored.task.updated_at = now.clone();
        stored.frontmatter.updated_at = Some(now);
        self.validate_stored_task(&stored)?;
        let mut tasks = self
            .load_tasks()?
            .into_iter()
            .map(|candidate| {
                if candidate.task.id == stored.task.id {
                    stored.task.clone()
                } else {
                    candidate.task
                }
            })
            .collect::<Vec<_>>();
        if !tasks.iter().any(|task| task.id == stored.task.id) {
            tasks.push(stored.task.clone());
        }
        let errors = validation::validate_dependencies(&tasks);
        if !errors.is_empty() {
            return Err(StoreError::Validation(errors));
        }
        self.write_validated_stored_task(&stored)?;
        self.load_task_file(&stored.path)
    }

    fn write_validated_stored_task(&self, stored: &StoredTask) -> StoreResult<()> {
        self.validate_stored_task(stored)?;
        self.write_task(stored)
    }

    fn validate_stored_task(&self, stored: &StoredTask) -> StoreResult<()> {
        let raw = write_task_file(&stored.frontmatter, &stored.body).map_err(|source| {
            StoreError::Frontmatter {
                path: stored.path.clone(),
                source,
            }
        })?;
        validation::validate_task_file(&raw, &stored.path.display().to_string())
            .map(|_| ())
            .map_err(StoreError::Validation)
    }

    pub fn validate(&self, _config: &Config) -> StoreResult<Vec<ValidationError>> {
        let mut tasks = Vec::new();
        let mut errors = Vec::new();
        for path in self.task_paths()? {
            let raw = fs::read_to_string(&path).map_err(|source| StoreError::Io {
                path: path.clone(),
                source,
            })?;
            match validation::validate_task_file(&raw, &path.display().to_string()) {
                Ok(task) => tasks.push(task),
                Err(mut task_errors) => errors.append(&mut task_errors),
            }
        }
        errors.extend(validation::validate_unique_ids(&tasks));
        errors.extend(validation::validate_dependencies(&tasks));
        Ok(errors)
    }

    fn load_task_file(&self, path: &Path) -> StoreResult<StoredTask> {
        let raw = fs::read_to_string(path).map_err(|source| StoreError::Io {
            path: path.to_path_buf(),
            source,
        })?;
        let (frontmatter, body) =
            parse_task_file(&raw).map_err(|source| StoreError::Frontmatter {
                path: path.to_path_buf(),
                source,
            })?;
        let task = validation::validate_task_file(&raw, &path.display().to_string())
            .map_err(StoreError::Validation)?;
        Ok(StoredTask {
            task,
            frontmatter,
            body: body.to_owned(),
            path: path.to_path_buf(),
        })
    }

    fn task_paths(&self) -> StoreResult<Vec<PathBuf>> {
        let tasks_dir = self.tasks_dir();
        if !tasks_dir.exists() {
            return Ok(Vec::new());
        }
        let mut paths = Vec::new();
        for entry in fs::read_dir(&tasks_dir).map_err(|source| StoreError::Io {
            path: tasks_dir.clone(),
            source,
        })? {
            let entry = entry.map_err(|source| StoreError::Io {
                path: tasks_dir.clone(),
                source,
            })?;
            let path = entry.path();
            if path.extension().and_then(|ext| ext.to_str()) == Some("md") {
                paths.push(path);
            }
        }
        paths.sort();
        Ok(paths)
    }

    fn ensure_unique_id(&self, id: &str) -> StoreResult<()> {
        if self.task_id_exists(id)? {
            Err(StoreError::DuplicateTaskId(id.to_owned()))
        } else {
            Ok(())
        }
    }

    fn task_id_exists(&self, id: &str) -> StoreResult<bool> {
        Ok(self.load_tasks()?.iter().any(|stored| stored.task.id == id))
    }

    fn unique_generated_task_id(&self, now: &str) -> StoreResult<String> {
        let base = generated_task_id(now);
        for suffix in 0..1000 {
            let candidate = if suffix == 0 {
                base.clone()
            } else {
                format!("{base}-{suffix}")
            };
            if !self.task_id_exists(&candidate)? {
                return Ok(candidate);
            }
        }
        Err(StoreError::PathCollision(base))
    }

    fn unique_task_path(&self, id: &str) -> StoreResult<PathBuf> {
        let stem = sanitize_file_stem(id);
        for suffix in 0..1000 {
            let filename = if suffix == 0 {
                format!("{stem}.md")
            } else {
                format!("{stem}-{suffix}.md")
            };
            let path = self.tasks_dir().join(filename);
            if !path.exists() {
                return Ok(path);
            }
        }
        Err(StoreError::PathCollision(id.to_owned()))
    }
}

pub fn default_config() -> Config {
    let mut config = Config::default();
    config.default_backend = "noop".to_owned();
    config.max_parallel = config.max_parallel.max(1);
    config.backends.insert(
        "noop".to_owned(),
        BackendConfig {
            kind: "noop".to_owned(),
        },
    );
    config.backends.insert(
        "shell".to_owned(),
        BackendConfig {
            kind: "shell".to_owned(),
        },
    );
    config.agents.insert(
        "coder".to_owned(),
        AgentConfig {
            backend: "noop".to_owned(),
            command: None,
        },
    );
    config.agents.insert(
        "reviewer".to_owned(),
        AgentConfig {
            backend: "noop".to_owned(),
            command: None,
        },
    );
    config
}

pub fn with_builtin_defaults(mut config: Config) -> Config {
    let defaults = default_config();
    if config.max_parallel == 0 {
        config.max_parallel = defaults.max_parallel;
    }
    config
        .backends
        .entry("noop".to_owned())
        .or_insert_with(|| BackendConfig {
            kind: "noop".to_owned(),
        });
    config
        .backends
        .entry("shell".to_owned())
        .or_insert_with(|| BackendConfig {
            kind: "shell".to_owned(),
        });
    if config.agents.is_empty() {
        config.agents = defaults.agents;
    }
    config
}

pub fn now_rfc3339() -> String {
    Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, true)
}

pub fn atomic_write(path: &Path, contents: &[u8]) -> StoreResult<()> {
    let parent = path.parent().unwrap_or_else(|| Path::new("."));
    fs::create_dir_all(parent).map_err(|source| StoreError::Io {
        path: parent.to_path_buf(),
        source,
    })?;
    let filename = path
        .file_name()
        .ok_or_else(|| StoreError::MissingFileName(path.to_path_buf()))?
        .to_string_lossy();
    let tmp = parent.join(format!(".{filename}.tmp"));
    fs::write(&tmp, contents).map_err(|source| StoreError::Io {
        path: tmp.clone(),
        source,
    })?;
    fs::rename(&tmp, path).map_err(|source| StoreError::Io {
        path: path.to_path_buf(),
        source,
    })
}

fn generated_task_id(now: &str) -> String {
    let compact: String = now
        .chars()
        .filter(|ch| ch.is_ascii_alphanumeric())
        .collect();
    format!("task-{compact}")
}

fn default_task_body() -> String {
    "## Goal\n\n\n## Context\n\n\n## Constraints\n\n\n## Acceptance\n\n".to_owned()
}

fn update_section_body(
    body: &str,
    section: TaskSectionName,
    text: &str,
    mode: SectionMode,
) -> String {
    let heading = format!("## {}", section.title());
    let lines = body.lines().collect::<Vec<_>>();
    let start = lines
        .iter()
        .position(|line| line.trim().eq_ignore_ascii_case(&heading));
    let normalized_text = text.trim_matches('\n');

    match start {
        Some(start) => {
            let end = lines
                .iter()
                .enumerate()
                .skip(start + 1)
                .find_map(|(index, line)| line.trim_start().starts_with("## ").then_some(index))
                .unwrap_or(lines.len());
            let mut out = String::new();
            for line in &lines[..=start] {
                out.push_str(line);
                out.push('\n');
            }
            out.push('\n');
            if mode == SectionMode::Append {
                for line in lines[start + 1..end]
                    .iter()
                    .skip_while(|line| line.trim().is_empty())
                {
                    out.push_str(line);
                    out.push('\n');
                }
                if !out.ends_with("\n\n") {
                    out.push('\n');
                }
            }
            if !normalized_text.is_empty() {
                out.push_str(normalized_text);
                out.push('\n');
            }
            if end < lines.len() {
                out.push('\n');
                for line in &lines[end..] {
                    out.push_str(line);
                    out.push('\n');
                }
            }
            out
        }
        None => {
            let mut out = body.trim_end_matches('\n').to_owned();
            if !out.is_empty() {
                out.push_str("\n\n");
            }
            out.push_str(&heading);
            out.push_str("\n\n");
            out.push_str(normalized_text);
            out.push('\n');
            out
        }
    }
}

fn sanitize_file_stem(id: &str) -> String {
    let mut out = String::with_capacity(id.len());
    for ch in id.chars() {
        if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
            out.push(ch);
        } else {
            out.push('-');
        }
    }
    if out.is_empty() {
        "task".to_owned()
    } else {
        out
    }
}
