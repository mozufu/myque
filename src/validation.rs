/// Structured validation for tasks and config.
///
/// All public functions return `Vec<ValidationError>` — callers receive every
/// problem in one pass rather than stopping at the first failure.
use crate::frontmatter::{FrontmatterError, extract_sections};
use crate::model::{Config, Status, Task};
use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Error taxonomy
// ---------------------------------------------------------------------------

/// Machine-readable category codes, mirroring the spec's error taxonomy.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorCode {
    /// `config.invalid_toml`
    ConfigInvalidToml,
    /// `task.invalid_toml`
    TaskInvalidToml,
    /// `task.missing_required_field`
    TaskMissingRequiredField,
    /// `task.invalid_status`
    TaskInvalidStatus,
    /// `task.duplicate_id`
    TaskDuplicateId,
    /// `task.missing_section`  (required Markdown section absent)
    TaskMissingSection,
    /// `task.empty_acceptance`
    TaskEmptyAcceptance,
    /// `deps.unknown_task`
    DepsUnknownTask,
    /// `deps.cycle`
    DepsCycle,
    /// `policy.missing_allowed_label`
    PolicyMissingAllowedLabel,
    /// `policy.blocked_label`
    PolicyBlockedLabel,
    /// `backend.unknown_backend`
    BackendUnknownBackend,
    /// `agent.unknown_agent`
    AgentUnknownAgent,
}

impl ErrorCode {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::ConfigInvalidToml => "config.invalid_toml",
            Self::TaskInvalidToml => "task.invalid_toml",
            Self::TaskMissingRequiredField => "task.missing_required_field",
            Self::TaskInvalidStatus => "task.invalid_status",
            Self::TaskDuplicateId => "task.duplicate_id",
            Self::TaskMissingSection => "task.missing_section",
            Self::TaskEmptyAcceptance => "task.empty_acceptance",
            Self::DepsUnknownTask => "deps.unknown_task",
            Self::DepsCycle => "deps.cycle",
            Self::PolicyMissingAllowedLabel => "policy.missing_allowed_label",
            Self::PolicyBlockedLabel => "policy.blocked_label",
            Self::BackendUnknownBackend => "backend.unknown_backend",
            Self::AgentUnknownAgent => "agent.unknown_agent",
        }
    }
}

impl std::fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// A single structured validation problem.
#[derive(Debug, Clone)]
pub struct ValidationError {
    pub code: ErrorCode,
    /// Task ID or file path context, when known.
    pub context: Option<String>,
    /// Human-readable description.
    pub message: String,
}

impl ValidationError {
    fn new(
        code: ErrorCode,
        context: impl Into<Option<String>>,
        message: impl Into<String>,
    ) -> Self {
        Self {
            code,
            context: context.into(),
            message: message.into(),
        }
    }
}

impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.context {
            Some(ctx) => write!(f, "[{}] {} — {}", ctx, self.code, self.message),
            None => write!(f, "{} — {}", self.code, self.message),
        }
    }
}

impl std::error::Error for ValidationError {}

// ---------------------------------------------------------------------------
// Config validation
// ---------------------------------------------------------------------------

/// Validate a raw config TOML string.
pub fn validate_config_toml(raw: &str) -> Result<Config, ValidationError> {
    toml::from_str::<Config>(raw).map_err(|e| {
        ValidationError::new(
            ErrorCode::ConfigInvalidToml,
            None,
            format!("config.toml parse error: {e}"),
        )
    })
}

// ---------------------------------------------------------------------------
// Task file validation pipeline
// ---------------------------------------------------------------------------

/// Validate a single task file given its raw content and source path/label.
///
/// Returns the strongly-typed [`Task`] on success, or one or more
/// [`ValidationError`]s on failure.
pub fn validate_task_file(raw: &str, source: &str) -> Result<Task, Vec<ValidationError>> {
    // Step 1 — split and parse frontmatter.
    let (fm, body) = match crate::frontmatter::parse_task_file(raw) {
        Ok(pair) => pair,
        Err(FrontmatterError::NoDelimiter) | Err(FrontmatterError::UnclosedDelimiter) => {
            return Err(vec![ValidationError::new(
                ErrorCode::TaskInvalidToml,
                source.to_owned(),
                "file does not contain valid +++ frontmatter delimiters",
            )]);
        }
        Err(FrontmatterError::InvalidToml(msg)) => {
            return Err(vec![ValidationError::new(
                ErrorCode::TaskInvalidToml,
                source.to_owned(),
                msg,
            )]);
        }
        Err(e) => {
            return Err(vec![ValidationError::new(
                ErrorCode::TaskInvalidToml,
                source.to_owned(),
                e.to_string(),
            )]);
        }
    };

    // Step 2 — collect all errors rather than short-circuiting.
    let mut errors: Vec<ValidationError> = Vec::new();

    // Determine context label: prefer the parsed id, fall back to source path.
    let ctx: String = fm.id.clone().unwrap_or_else(|| source.to_owned());

    // Step 3 — required fields.
    for field in fm.missing_required() {
        errors.push(ValidationError::new(
            ErrorCode::TaskMissingRequiredField,
            ctx.clone(),
            format!("required field `{field}` is missing"),
        ));
    }

    // Step 4 — status validity (only if present).
    let status_opt = fm.status.as_deref().and_then(|s| {
        let parsed = Status::parse_str(s);
        if parsed.is_none() {
            errors.push(ValidationError::new(
                ErrorCode::TaskInvalidStatus,
                ctx.clone(),
                format!(
                    "unknown status `{s}`; expected one of: {}",
                    Status::all().join(", ")
                ),
            ));
        }
        parsed
    });

    // Step 5 — Markdown sections.
    let sections = extract_sections(body);

    // `allowed_auto_dispatch = true` requires a non-empty ## Acceptance section.
    if fm.allowed_auto_dispatch == Some(true) && !sections.has_non_empty_acceptance() {
        errors.push(ValidationError::new(
            ErrorCode::TaskEmptyAcceptance,
            ctx.clone(),
            "`allowed_auto_dispatch = true` requires a non-empty ## Acceptance section",
        ));
    }

    // Step 6 — If we have errors at this point, return them before constructing Task.
    if !errors.is_empty() {
        return Err(errors);
    }

    // Step 7 — Construct the validated Task.  All required fields are guaranteed
    // present at this point (errors is empty).
    let task = Task {
        id: fm.id.unwrap(),
        title: fm.title.unwrap(),
        status: status_opt.unwrap(),
        priority: fm.priority.unwrap(),
        order: fm.order.unwrap(),
        labels: fm.labels.unwrap(),
        agent: fm.agent.unwrap(),
        backend: fm.backend.unwrap(),
        depends_on: fm.depends_on.unwrap(),
        allowed_auto_dispatch: fm.allowed_auto_dispatch.unwrap(),
        attempts: fm.attempts.unwrap(),
        max_attempts: fm.max_attempts.unwrap(),
        created_at: fm.created_at.unwrap(),
        updated_at: fm.updated_at.unwrap(),
        milestone: fm.milestone,
        risk: fm.risk,
        review_required: fm.review_required,
        paths: fm.paths,
        last_run_id: fm.last_run_id,
        assigned_at: fm.assigned_at,
        completed_at: fm.completed_at,
        failure_reason: fm.failure_reason,
        sections,
    };

    Ok(task)
}

// ---------------------------------------------------------------------------
// Cross-task validations
// ---------------------------------------------------------------------------

/// Validate dependency references across a slice of already-parsed tasks.
///
/// Checks:
/// - All `depends_on` IDs exist in the task set.
/// - The dependency graph is acyclic.
pub fn validate_dependencies(tasks: &[Task]) -> Vec<ValidationError> {
    use std::collections::{HashMap, HashSet};

    let id_set: HashSet<&str> = tasks.iter().map(|t| t.id.as_str()).collect();
    let mut errors = Vec::new();

    // Unknown dependency targets.
    for task in tasks {
        for dep in &task.depends_on {
            if !id_set.contains(dep.as_str()) {
                errors.push(ValidationError::new(
                    ErrorCode::DepsUnknownTask,
                    task.id.clone(),
                    format!("depends_on references unknown task `{dep}`"),
                ));
            }
        }
    }

    // Cycle detection via DFS colouring (0=white, 1=grey, 2=black).
    let adj: HashMap<&str, &[String]> = tasks
        .iter()
        .map(|t| (t.id.as_str(), t.depends_on.as_slice()))
        .collect();

    let mut colour: HashMap<&str, u8> = HashMap::new();

    for task in tasks {
        if !colour.contains_key(task.id.as_str()) {
            dfs_cycle(task.id.as_str(), &adj, &mut colour, &mut errors);
        }
    }

    errors
}

fn dfs_cycle<'a>(
    node: &'a str,
    adj: &HashMap<&'a str, &'a [String]>,
    colour: &mut std::collections::HashMap<&'a str, u8>,
    errors: &mut Vec<ValidationError>,
) {
    colour.insert(node, 1); // grey = in stack
    if let Some(deps) = adj.get(node) {
        for dep in *deps {
            let dep_str = dep.as_str();
            match colour.get(dep_str).copied() {
                Some(1) => {
                    // Back edge → cycle.
                    errors.push(ValidationError::new(
                        ErrorCode::DepsCycle,
                        node.to_owned(),
                        format!("dependency cycle detected: `{node}` → `{dep}`"),
                    ));
                }
                Some(2) => {} // already fully explored
                _ => {
                    if adj.contains_key(dep_str) {
                        dfs_cycle(dep_str, adj, colour, errors);
                    }
                }
            }
        }
    }
    colour.insert(node, 2); // black = done
}

/// Check that no two tasks in the slice share the same ID.
pub fn validate_unique_ids(tasks: &[Task]) -> Vec<ValidationError> {
    let mut seen = std::collections::HashMap::<&str, usize>::new();
    let mut errors = Vec::new();
    for task in tasks {
        let count = seen.entry(task.id.as_str()).or_insert(0);
        *count += 1;
        if *count == 2 {
            errors.push(ValidationError::new(
                ErrorCode::TaskDuplicateId,
                task.id.clone(),
                format!("task id `{}` appears more than once", task.id),
            ));
        }
    }
    errors
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::Status;

    fn make_raw(overrides: &[(&str, &str)], extra_fields: &str, body: &str) -> String {
        let mut fm = vec![
            ("id", "\"t-001\""),
            ("title", "\"Test task\""),
            ("status", "\"ready\""),
            ("priority", "1"),
            ("order", "100"),
            ("labels", "[\"safe-auto\"]"),
            ("agent", "\"coder\""),
            ("backend", "\"noop\""),
            ("depends_on", "[]"),
            ("allowed_auto_dispatch", "true"),
            ("attempts", "0"),
            ("max_attempts", "2"),
            ("created_at", "\"2026-06-22T00:00:00Z\""),
            ("updated_at", "\"2026-06-22T00:00:00Z\""),
        ];
        for (k, v) in overrides {
            if let Some(entry) = fm.iter_mut().find(|(fk, _)| fk == k) {
                *entry = (k, v);
            }
        }
        let fields: String = fm
            .iter()
            .map(|(k, v)| format!("{k} = {v}\n"))
            .collect::<String>();
        format!("+++\n{fields}{extra_fields}+++\n\n{body}")
    }

    #[test]
    fn valid_task_parses() {
        let raw = make_raw(
            &[],
            "",
            "## Goal\n\nDo something.\n\n## Acceptance\n\n- Item.\n",
        );
        let task = validate_task_file(&raw, "t-001.md").unwrap();
        assert_eq!(task.id, "t-001");
        assert_eq!(task.status, Status::Ready);
    }

    #[test]
    fn missing_required_field_error() {
        // Omit `title` by replacing it with nothing.
        let raw = "+++\nid = \"t-001\"\nstatus = \"ready\"\npriority = 1\norder = 100\nlabels = []\nagent = \"coder\"\nbackend = \"noop\"\ndepends_on = []\nallowed_auto_dispatch = false\nattempts = 0\nmax_attempts = 2\ncreated_at = \"2026-06-22T00:00:00Z\"\nupdated_at = \"2026-06-22T00:00:00Z\"\n+++\n\n## Goal\n\nHi.\n";
        let errs = validate_task_file(raw, "t-001.md").unwrap_err();
        assert!(
            errs.iter()
                .any(|e| e.code == ErrorCode::TaskMissingRequiredField
                    && e.message.contains("title"))
        );
    }

    #[test]
    fn invalid_status_error() {
        let raw = make_raw(
            &[("status", "\"purple\"")],
            "",
            "## Goal\n\nHi.\n## Acceptance\n\n- x.\n",
        );
        let errs = validate_task_file(&raw, "t-001.md").unwrap_err();
        assert!(errs.iter().any(|e| e.code == ErrorCode::TaskInvalidStatus));
    }

    #[test]
    fn malformed_toml_error() {
        let raw = "+++\nnot valid toml [\n+++\n\nbody";
        let errs = validate_task_file(raw, "bad.md").unwrap_err();
        assert!(errs.iter().any(|e| e.code == ErrorCode::TaskInvalidToml));
    }

    #[test]
    fn no_delimiter_error() {
        let raw = "## Goal\n\nNo frontmatter.";
        let errs = validate_task_file(raw, "nodm.md").unwrap_err();
        assert!(errs.iter().any(|e| e.code == ErrorCode::TaskInvalidToml));
    }

    #[test]
    fn empty_acceptance_error_when_auto_dispatch() {
        // allowed_auto_dispatch = true but no ## Acceptance content.
        let raw = make_raw(&[], "", "## Goal\n\nDo it.\n");
        let errs = validate_task_file(&raw, "t-001.md").unwrap_err();
        assert!(
            errs.iter()
                .any(|e| e.code == ErrorCode::TaskEmptyAcceptance)
        );
    }

    #[test]
    fn no_acceptance_error_when_auto_dispatch_false() {
        let raw = make_raw(
            &[("allowed_auto_dispatch", "false")],
            "",
            "## Goal\n\nDo it.\n",
        );
        validate_task_file(&raw, "t-001.md")
            .expect("should pass without acceptance when auto_dispatch false");
    }

    #[test]
    fn dependency_unknown_task() {
        let raw = make_raw(
            &[
                ("depends_on", "[\"nonexistent\"]"),
                ("allowed_auto_dispatch", "false"),
            ],
            "",
            "## Goal\n\nHi.\n",
        );
        let task = validate_task_file(&raw, "t-001.md").unwrap();
        let errs = validate_dependencies(&[task]);
        assert!(errs.iter().any(|e| e.code == ErrorCode::DepsUnknownTask));
    }

    #[test]
    fn dependency_cycle_detected() {
        fn make_task(id: &str, deps: &[&str]) -> Task {
            Task {
                id: id.into(),
                title: "x".into(),
                status: Status::Ready,
                priority: 1,
                order: 1,
                labels: vec![],
                agent: "coder".into(),
                backend: "noop".into(),
                depends_on: deps.iter().map(|s| s.to_string()).collect(),
                allowed_auto_dispatch: false,
                attempts: 0,
                max_attempts: 2,
                created_at: "2026-06-22T00:00:00Z".into(),
                updated_at: "2026-06-22T00:00:00Z".into(),
                milestone: None,
                risk: None,
                review_required: None,
                paths: None,
                last_run_id: None,
                assigned_at: None,
                completed_at: None,
                failure_reason: None,
                sections: crate::model::TaskSections::default(),
            }
        }
        let tasks = vec![make_task("a", &["b"]), make_task("b", &["a"])];
        let errs = validate_dependencies(&tasks);
        assert!(errs.iter().any(|e| e.code == ErrorCode::DepsCycle));
    }

    #[test]
    fn duplicate_ids_detected() {
        fn quick_task(id: &str) -> Task {
            Task {
                id: id.into(),
                title: "x".into(),
                status: Status::Backlog,
                priority: 1,
                order: 1,
                labels: vec![],
                agent: "coder".into(),
                backend: "noop".into(),
                depends_on: vec![],
                allowed_auto_dispatch: false,
                attempts: 0,
                max_attempts: 2,
                created_at: "2026-06-22T00:00:00Z".into(),
                updated_at: "2026-06-22T00:00:00Z".into(),
                milestone: None,
                risk: None,
                review_required: None,
                paths: None,
                last_run_id: None,
                assigned_at: None,
                completed_at: None,
                failure_reason: None,
                sections: crate::model::TaskSections::default(),
            }
        }
        let tasks = vec![quick_task("dup"), quick_task("dup"), quick_task("unique")];
        let errs = validate_unique_ids(&tasks);
        assert_eq!(errs.len(), 1);
        assert_eq!(errs[0].code, ErrorCode::TaskDuplicateId);
    }

    #[test]
    fn valid_config_parses() {
        let raw = r#"
default_backend = "noop"
max_parallel = 2

[policy]
auto_dispatch = true
require_allowed_label = true
allowed_labels = ["safe-auto"]
blocked_labels = ["dangerous", "needs-human", "destructive"]
require_acceptance_section = true
require_allowed_auto_dispatch = true
max_attempts_default = 2
agents_may_mark_done = false

[agents.coder]
backend = "noop"
"#;
        let cfg = validate_config_toml(raw).unwrap();
        assert_eq!(cfg.default_backend, "noop");
        assert_eq!(cfg.policy.allowed_labels, vec!["safe-auto"]);
    }

    #[test]
    fn invalid_config_toml_error() {
        let err = validate_config_toml("not valid [").unwrap_err();
        assert_eq!(err.code, ErrorCode::ConfigInvalidToml);
    }

    #[test]
    fn optional_fields_accepted() {
        let raw = make_raw(
            &[("allowed_auto_dispatch", "false")],
            "last_run_id = \"run-001\"\nassigned_at = \"2026-06-22T01:00:00Z\"\n",
            "## Goal\n\nHi.\n",
        );
        let task = validate_task_file(&raw, "t-001.md").unwrap();
        assert_eq!(task.last_run_id.as_deref(), Some("run-001"));
        assert_eq!(task.assigned_at.as_deref(), Some("2026-06-22T01:00:00Z"));
    }
}
