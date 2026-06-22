use serde::{Deserialize, Serialize};
use std::fmt;

// ---------------------------------------------------------------------------
// Status
// ---------------------------------------------------------------------------

/// All valid lifecycle statuses for a task.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Status {
    Backlog,
    Ready,
    Blocked,
    Running,
    Review,
    Done,
    Failed,
    Cancelled,
}

impl Status {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Backlog => "backlog",
            Self::Ready => "ready",
            Self::Blocked => "blocked",
            Self::Running => "running",
            Self::Review => "review",
            Self::Done => "done",
            Self::Failed => "failed",
            Self::Cancelled => "cancelled",
        }
    }

    /// Parse a lowercase string into a `Status`, returning `None` for unknown values.
    pub fn parse_str(s: &str) -> Option<Self> {
        match s {
            "backlog" => Some(Self::Backlog),
            "ready" => Some(Self::Ready),
            "blocked" => Some(Self::Blocked),
            "running" => Some(Self::Running),
            "review" => Some(Self::Review),
            "done" => Some(Self::Done),
            "failed" => Some(Self::Failed),
            "cancelled" => Some(Self::Cancelled),
            _ => None,
        }
    }

    /// All canonical status values, useful for validation error messages.
    pub fn all() -> &'static [&'static str] {
        &[
            "backlog",
            "ready",
            "blocked",
            "running",
            "review",
            "done",
            "failed",
            "cancelled",
        ]
    }

    /// Returns `true` if this status terminates the lifecycle.
    pub fn is_terminal(&self) -> bool {
        matches!(self, Self::Done | Self::Cancelled)
    }

    /// Returns `true` if the task can potentially be dispatched from this status.
    pub fn is_dispatchable(&self) -> bool {
        matches!(self, Self::Ready)
    }
}

impl fmt::Display for Status {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl std::str::FromStr for Status {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse_str(s).ok_or_else(|| {
            format!(
                "unknown status `{s}`; expected one of: {}",
                Self::all().join(", ")
            )
        })
    }
}

// ---------------------------------------------------------------------------
// TaskFrontmatter — raw TOML-deserializable record
// ---------------------------------------------------------------------------

/// Represents all frontmatter fields parsed from the `+++` TOML block.
///
/// Required fields will be `Option<_>` during deserialization so that we can
/// produce precise `missing_required_field` errors rather than a generic
/// serde parse error.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskFrontmatter {
    // Required fields ---------------------------------------------------------
    pub id: Option<String>,
    pub title: Option<String>,
    pub status: Option<String>, // validated separately via Status::from_str
    pub priority: Option<i64>,
    pub order: Option<i64>,
    pub labels: Option<Vec<String>>,
    pub agent: Option<String>,
    pub backend: Option<String>,
    pub depends_on: Option<Vec<String>>,
    pub allowed_auto_dispatch: Option<bool>,
    pub attempts: Option<u32>,
    pub max_attempts: Option<u32>,
    pub created_at: Option<String>,
    pub updated_at: Option<String>,

    // Optional fields ---------------------------------------------------------
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub milestone: Option<String>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub risk: Option<String>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub review_required: Option<bool>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub paths: Option<Vec<String>>,

    /// ID of the most recent dispatch run record.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub last_run_id: Option<String>,

    /// Timestamp when the task was most recently assigned to a backend.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub assigned_at: Option<String>,

    /// Timestamp when the task reached a terminal status.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub completed_at: Option<String>,

    /// Human-readable failure explanation set on `failed`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub failure_reason: Option<String>,
}

impl TaskFrontmatter {
    /// List of required field names; used by validation.
    pub const REQUIRED: &'static [&'static str] = &[
        "id",
        "title",
        "status",
        "priority",
        "order",
        "labels",
        "agent",
        "backend",
        "depends_on",
        "allowed_auto_dispatch",
        "attempts",
        "max_attempts",
        "created_at",
        "updated_at",
    ];

    /// Returns the names of required fields that are `None`.
    pub fn missing_required(&self) -> Vec<&'static str> {
        let mut missing = Vec::new();
        if self.id.is_none() {
            missing.push("id");
        }
        if self.title.is_none() {
            missing.push("title");
        }
        if self.status.is_none() {
            missing.push("status");
        }
        if self.priority.is_none() {
            missing.push("priority");
        }
        if self.order.is_none() {
            missing.push("order");
        }
        if self.labels.is_none() {
            missing.push("labels");
        }
        if self.agent.is_none() {
            missing.push("agent");
        }
        if self.backend.is_none() {
            missing.push("backend");
        }
        if self.depends_on.is_none() {
            missing.push("depends_on");
        }
        if self.allowed_auto_dispatch.is_none() {
            missing.push("allowed_auto_dispatch");
        }
        if self.attempts.is_none() {
            missing.push("attempts");
        }
        if self.max_attempts.is_none() {
            missing.push("max_attempts");
        }
        if self.created_at.is_none() {
            missing.push("created_at");
        }
        if self.updated_at.is_none() {
            missing.push("updated_at");
        }
        missing
    }
}

// ---------------------------------------------------------------------------
// Task — validated, strongly-typed task model
// ---------------------------------------------------------------------------

/// A fully parsed and validated task.
///
/// All required fields are non-optional here; construction is only possible
/// through `TaskFrontmatter` validation (see `crate::validation`).
#[derive(Debug, Clone)]
pub struct Task {
    // Required fields
    pub id: String,
    pub title: String,
    pub status: Status,
    pub priority: i64,
    pub order: i64,
    pub labels: Vec<String>,
    pub agent: String,
    pub backend: String,
    pub depends_on: Vec<String>,
    pub allowed_auto_dispatch: bool,
    pub attempts: u32,
    pub max_attempts: u32,
    pub created_at: String,
    pub updated_at: String,

    // Optional fields
    pub milestone: Option<String>,
    pub risk: Option<String>,
    pub review_required: Option<bool>,
    pub paths: Option<Vec<String>>,
    pub last_run_id: Option<String>,
    pub assigned_at: Option<String>,
    pub completed_at: Option<String>,
    pub failure_reason: Option<String>,

    /// Parsed Markdown sections from the task body.
    pub sections: TaskSections,
}

/// Extracted Markdown sections from the task body.
#[derive(Debug, Clone, Default)]
pub struct TaskSections {
    pub goal: Option<String>,
    pub context: Option<String>,
    pub constraints: Option<String>,
    pub acceptance: Option<String>,
    pub files: Option<String>,
    pub notes: Option<String>,
}

impl TaskSections {
    /// Returns `true` if the `## Acceptance` section is present and non-empty.
    pub fn has_non_empty_acceptance(&self) -> bool {
        self.acceptance
            .as_deref()
            .map(|s| !s.trim().is_empty())
            .unwrap_or(false)
    }
}

// ---------------------------------------------------------------------------
// PolicyConfig
// ---------------------------------------------------------------------------

/// Dispatch policy from `[policy]` in config.toml.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PolicyConfig {
    #[serde(default = "default_true")]
    pub auto_dispatch: bool,

    #[serde(default = "default_true")]
    pub require_allowed_label: bool,

    #[serde(default = "default_allowed_labels")]
    pub allowed_labels: Vec<String>,

    #[serde(default = "default_blocked_labels")]
    pub blocked_labels: Vec<String>,

    #[serde(default = "default_true")]
    pub require_acceptance_section: bool,

    #[serde(default = "default_true")]
    pub require_allowed_auto_dispatch: bool,

    #[serde(default = "default_max_attempts")]
    pub max_attempts_default: u32,

    #[serde(default)]
    pub agents_may_mark_done: bool,
}

impl Default for PolicyConfig {
    fn default() -> Self {
        Self {
            auto_dispatch: true,
            require_allowed_label: true,
            allowed_labels: default_allowed_labels(),
            blocked_labels: default_blocked_labels(),
            require_acceptance_section: true,
            require_allowed_auto_dispatch: true,
            max_attempts_default: 2,
            agents_may_mark_done: false,
        }
    }
}

// ---------------------------------------------------------------------------
// AgentConfig / BackendConfig
// ---------------------------------------------------------------------------

/// Per-agent configuration block from `[agents.<name>]`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentConfig {
    pub backend: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub command: Option<String>,
}

/// Per-backend configuration block from `[backends.<name>]`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BackendConfig {
    pub kind: String,
}

// ---------------------------------------------------------------------------
// Config — root config.toml structure
// ---------------------------------------------------------------------------

/// Represents `.myque/config.toml`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    #[serde(default = "default_backend")]
    pub default_backend: String,

    #[serde(default = "default_max_parallel")]
    pub max_parallel: u32,

    #[serde(default)]
    pub policy: PolicyConfig,

    #[serde(default)]
    pub agents: std::collections::HashMap<String, AgentConfig>,

    #[serde(default)]
    pub backends: std::collections::HashMap<String, BackendConfig>,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            default_backend: "noop".into(),
            max_parallel: 1,
            policy: PolicyConfig::default(),
            agents: std::collections::HashMap::new(),
            backends: std::collections::HashMap::new(),
        }
    }
}

// ---------------------------------------------------------------------------
// Helpers for serde defaults
// ---------------------------------------------------------------------------

fn default_true() -> bool {
    true
}
fn default_backend() -> String {
    "noop".into()
}
fn default_max_parallel() -> u32 {
    1
}
fn default_max_attempts() -> u32 {
    2
}
fn default_allowed_labels() -> Vec<String> {
    vec!["safe-auto".into()]
}
fn default_blocked_labels() -> Vec<String> {
    vec![
        "dangerous".into(),
        "needs-human".into(),
        "destructive".into(),
    ]
}
