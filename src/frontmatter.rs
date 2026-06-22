/// TOML frontmatter helpers for task Markdown files.
///
/// Task files use `+++` delimiters (not YAML `---`):
///
/// ```text
/// +++
/// id = "task-2026-06-22-001"
/// title = "Example"
/// ...
/// +++
///
/// ## Goal
/// ...
/// ```
use crate::model::{TaskFrontmatter, TaskSections};

// ---------------------------------------------------------------------------
// Parsing
// ---------------------------------------------------------------------------

/// Splits a raw Markdown file into its TOML frontmatter string and Markdown body.
///
/// Returns `Err` if the file does not start with `+++` or the closing `+++`
/// cannot be found.
pub fn split_frontmatter(raw: &str) -> Result<(&str, &str), FrontmatterError> {
    let raw = raw.trim_start_matches('\u{feff}'); // strip BOM if present

    if !raw.starts_with("+++") {
        return Err(FrontmatterError::NoDelimiter);
    }

    // Advance past the opening `+++` and optional newline.
    let after_open = &raw["+++".len()..];
    let after_open = after_open
        .strip_prefix('\n')
        .or_else(|| after_open.strip_prefix("\r\n"))
        .unwrap_or(after_open);

    // Find the closing `+++` on its own line.
    let close_marker = "\n+++";
    let close_pos = after_open
        .find(close_marker)
        .ok_or(FrontmatterError::UnclosedDelimiter)?;

    let toml_src = &after_open[..close_pos];
    let rest = &after_open[close_pos + close_marker.len()..];

    // Strip leading newline from body so callers get clean Markdown.
    let body = rest
        .strip_prefix('\n')
        .or_else(|| rest.strip_prefix("\r\n"))
        .unwrap_or(rest);

    Ok((toml_src, body))
}

/// Parses the frontmatter string (TOML only, no delimiters) into
/// [`TaskFrontmatter`].
pub fn parse_frontmatter(toml_src: &str) -> Result<TaskFrontmatter, FrontmatterError> {
    toml::from_str(toml_src).map_err(|e| FrontmatterError::InvalidToml(e.to_string()))
}

/// Combines `split_frontmatter` and `parse_frontmatter` in one step.
///
/// Returns `(TaskFrontmatter, body_str)` on success.
pub fn parse_task_file(raw: &str) -> Result<(TaskFrontmatter, &str), FrontmatterError> {
    let (toml_src, body) = split_frontmatter(raw)?;
    let fm = parse_frontmatter(toml_src)?;
    Ok((fm, body))
}

// ---------------------------------------------------------------------------
// Writing / round-trip
// ---------------------------------------------------------------------------

/// Serialises a [`TaskFrontmatter`] back to TOML and rebuilds the full file
/// content, preserving the original Markdown body verbatim.
pub fn write_task_file(fm: &TaskFrontmatter, body: &str) -> Result<String, FrontmatterError> {
    let toml_src =
        toml::to_string(fm).map_err(|e| FrontmatterError::SerializeError(e.to_string()))?;
    Ok(format!("+++\n{toml_src}+++\n\n{body}"))
}

// ---------------------------------------------------------------------------
// Markdown section extraction
// ---------------------------------------------------------------------------

/// Parses the Markdown body into labelled sections recognised by the spec.
///
/// Sections are delimited by level-2 headings (`## Name`).  Any heading not
/// in the recognised set is silently skipped (its text is not captured).
pub fn extract_sections(body: &str) -> TaskSections {
    let mut sections = TaskSections::default();

    // We track the current section name and buffer its content.
    let mut current: Option<&str> = None;
    let mut buf = String::new();

    for line in body.lines() {
        if let Some(heading) = parse_h2(line) {
            // Flush previous section.
            flush_section(&mut sections, current, &buf);
            buf.clear();
            current = recognised_section(heading);
        } else if current.is_some() {
            buf.push_str(line);
            buf.push('\n');
        }
    }

    // Flush last section.
    flush_section(&mut sections, current, &buf);

    sections
}

// ---------------------------------------------------------------------------
// Internals
// ---------------------------------------------------------------------------

fn parse_h2(line: &str) -> Option<&str> {
    let rest = line.strip_prefix("## ")?;
    Some(rest.trim())
}

/// Maps a heading title to the canonical section key (case-insensitive).
fn recognised_section(heading: &str) -> Option<&'static str> {
    match heading.to_lowercase().as_str() {
        "goal" => Some("goal"),
        "context" => Some("context"),
        "constraints" => Some("constraints"),
        "acceptance" => Some("acceptance"),
        "files" => Some("files"),
        "notes" => Some("notes"),
        _ => None,
    }
}

fn flush_section(sections: &mut TaskSections, key: Option<&str>, buf: &str) {
    let key = match key {
        Some(k) => k,
        None => return,
    };
    let trimmed = buf.trim().to_owned();
    let value = if trimmed.is_empty() {
        None
    } else {
        Some(trimmed)
    };
    match key {
        "goal" => sections.goal = value,
        "context" => sections.context = value,
        "constraints" => sections.constraints = value,
        "acceptance" => sections.acceptance = value,
        "files" => sections.files = value,
        "notes" => sections.notes = value,
        _ => {}
    }
}

// ---------------------------------------------------------------------------
// Error type
// ---------------------------------------------------------------------------

/// Errors that can arise during frontmatter parsing or serialisation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FrontmatterError {
    /// File does not begin with `+++`.
    NoDelimiter,
    /// Opening `+++` found but no closing `+++`.
    UnclosedDelimiter,
    /// TOML inside the delimiters is syntactically invalid.
    InvalidToml(String),
    /// TOML serialisation failed (should only happen with non-serialisable data).
    SerializeError(String),
}

impl std::fmt::Display for FrontmatterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoDelimiter => write!(f, "file does not start with +++"),
            Self::UnclosedDelimiter => write!(f, "opening +++ has no closing +++"),
            Self::InvalidToml(msg) => write!(f, "invalid TOML in frontmatter: {msg}"),
            Self::SerializeError(msg) => write!(f, "failed to serialize frontmatter: {msg}"),
        }
    }
}

impl std::error::Error for FrontmatterError {}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    const MINIMAL_TASK: &str = r#"+++
id = "t-001"
title = "Test task"
status = "ready"
priority = 1
order = 100
labels = ["safe-auto"]
agent = "coder"
backend = "noop"
depends_on = []
allowed_auto_dispatch = true
attempts = 0
max_attempts = 2
created_at = "2026-06-22T00:00:00Z"
updated_at = "2026-06-22T00:00:00Z"
+++

## Goal

Do something useful.

## Acceptance

- Task passes all checks.
"#;

    #[test]
    fn split_valid() {
        let (toml_src, body) = split_frontmatter(MINIMAL_TASK).unwrap();
        assert!(toml_src.contains("id = \"t-001\""));
        assert!(body.contains("## Goal"));
    }

    #[test]
    fn parse_valid() {
        let (fm, _body) = parse_task_file(MINIMAL_TASK).unwrap();
        assert_eq!(fm.id.as_deref(), Some("t-001"));
        assert_eq!(fm.status.as_deref(), Some("ready"));
        assert_eq!(fm.allowed_auto_dispatch, Some(true));
    }

    #[test]
    fn no_delimiter_error() {
        let err = split_frontmatter("## Goal\nNo frontmatter.").unwrap_err();
        assert_eq!(err, FrontmatterError::NoDelimiter);
    }

    #[test]
    fn unclosed_delimiter_error() {
        let err = split_frontmatter("+++\nid = \"x\"\n").unwrap_err();
        assert_eq!(err, FrontmatterError::UnclosedDelimiter);
    }

    #[test]
    fn invalid_toml_error() {
        let raw = "+++\nnot = valid toml [\n+++\n\nbody";
        let err = parse_task_file(raw).unwrap_err();
        assert!(matches!(err, FrontmatterError::InvalidToml(_)));
    }

    #[test]
    fn section_extraction() {
        let (_, body) = parse_task_file(MINIMAL_TASK).unwrap();
        let secs = extract_sections(body);
        assert!(secs.goal.is_some());
        assert!(secs.has_non_empty_acceptance());
        assert!(secs.context.is_none());
    }

    #[test]
    fn round_trip_preserves_body() {
        let (fm, body) = parse_task_file(MINIMAL_TASK).unwrap();
        let rebuilt = write_task_file(&fm, body).unwrap();
        let (fm2, body2) = parse_task_file(&rebuilt).unwrap();
        assert_eq!(fm2.id, fm.id);
        assert_eq!(body2.trim(), body.trim());
    }

    #[test]
    fn optional_last_run_id_roundtrip() {
        let mut raw = MINIMAL_TASK.to_owned();
        // Insert last_run_id into frontmatter.
        raw = raw.replace(
            "updated_at = \"2026-06-22T00:00:00Z\"",
            "updated_at = \"2026-06-22T00:00:00Z\"\nlast_run_id = \"run-001\"",
        );
        let (fm, _) = parse_task_file(&raw).unwrap();
        assert_eq!(fm.last_run_id.as_deref(), Some("run-001"));
    }
}
