use myque::board::{board_columns, render_board};
use myque::cli::run_with_args;
use myque::model::{Config, Status, Task, TaskSections};
use myque::store::TaskStore;
use std::fs;
use tempfile::TempDir;

fn task(id: &str, status: Status, priority: i64, order: i64, created_at: &str) -> Task {
    Task {
        id: id.to_owned(),
        title: format!("Title {id}"),
        status,
        priority,
        order,
        labels: Vec::new(),
        agent: "coder".to_owned(),
        backend: "noop".to_owned(),
        depends_on: Vec::new(),
        allowed_auto_dispatch: false,
        attempts: 0,
        max_attempts: 2,
        created_at: created_at.to_owned(),
        updated_at: created_at.to_owned(),
        milestone: None,
        risk: None,
        review_required: None,
        paths: None,
        last_run_id: None,
        assigned_at: None,
        completed_at: None,
        failure_reason: None,
        sections: TaskSections::default(),
    }
}

fn write_task(
    store: &TaskStore,
    id: &str,
    title: &str,
    status: &str,
    labels: &[&str],
    depends_on: &[&str],
    allowed_auto_dispatch: bool,
) -> std::path::PathBuf {
    let labels = labels
        .iter()
        .map(|label| format!("\"{label}\""))
        .collect::<Vec<_>>()
        .join(", ");
    let depends_on = depends_on
        .iter()
        .map(|dependency| format!("\"{dependency}\""))
        .collect::<Vec<_>>()
        .join(", ");
    let raw = format!(
        r#"+++
id = "{id}"
title = "{title}"
status = "{status}"
priority = 1
order = 100
labels = [{labels}]
agent = "coder"
backend = "noop"
depends_on = [{depends_on}]
allowed_auto_dispatch = {allowed_auto_dispatch}
attempts = 0
max_attempts = 2
created_at = "2026-06-22T00:00:00Z"
updated_at = "2026-06-22T00:00:00Z"
+++

## Goal

Finish {title}.

## Context

Test fixture context.

## Constraints

Stay local.

## Acceptance

- Observable result exists.
"#
    );
    let path = store.tasks_dir().join(format!("{id}.md"));
    fs::write(&path, raw).unwrap();
    path
}

fn read_task_files(store: &TaskStore) -> Vec<(String, String)> {
    let mut files: Vec<(String, String)> = fs::read_dir(store.tasks_dir())
        .unwrap()
        .map(|entry| {
            let path = entry.unwrap().path();
            (
                path.display().to_string(),
                fs::read_to_string(path).unwrap(),
            )
        })
        .collect();
    files.sort_by(|left, right| left.0.cmp(&right.0));
    files
}

#[test]
fn board_groups_and_sorts_without_mutating_files() {
    let tmp = TempDir::new().unwrap();
    let store = TaskStore::new(tmp.path());
    store.init(false).unwrap();
    let config = Config::load_or_default(tmp.path()).unwrap();

    run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "new",
        "Later",
        "--id",
        "task-later",
        "--status",
        "ready",
        "--priority",
        "2",
        "--order",
        "100",
    ])
    .unwrap();
    run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "new",
        "First",
        "--id",
        "task-first",
        "--status",
        "ready",
        "--priority",
        "1",
        "--order",
        "200",
    ])
    .unwrap();
    run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "new",
        "Backlog",
        "--id",
        "task-backlog",
        "--status",
        "backlog",
    ])
    .unwrap();

    let mut before: Vec<(String, String)> = fs::read_dir(store.tasks_dir())
        .unwrap()
        .map(|entry| {
            let path = entry.unwrap().path();
            (
                path.display().to_string(),
                fs::read_to_string(path).unwrap(),
            )
        })
        .collect();
    before.sort_by(|left, right| left.0.cmp(&right.0));

    let output = run_with_args(["myque", "--root", tmp.path().to_str().unwrap(), "board"]).unwrap();
    assert!(output.contains("READY"));
    assert!(output.contains("BACKLOG"));
    assert!(output.find("task-first").unwrap() < output.find("task-later").unwrap());

    let mut after: Vec<(String, String)> = fs::read_dir(store.tasks_dir())
        .unwrap()
        .map(|entry| {
            let path = entry.unwrap().path();
            (
                path.display().to_string(),
                fs::read_to_string(path).unwrap(),
            )
        })
        .collect();
    after.sort_by(|left, right| left.0.cmp(&right.0));
    assert_eq!(before, after);

    let errors = store.validate(&config).unwrap();
    assert!(
        errors.is_empty(),
        "new task files should validate: {errors:?}"
    );
}

#[test]
fn board_columns_sort_by_priority_order_created_at() {
    let later_created = task(
        "task-later-created",
        Status::Ready,
        1,
        1,
        "2026-06-22T12:01:00Z",
    );
    let earlier_created = task(
        "task-earlier-created",
        Status::Ready,
        1,
        1,
        "2026-06-22T12:00:00Z",
    );
    let lower_order = task(
        "task-lower-order",
        Status::Ready,
        1,
        0,
        "2026-06-22T12:02:00Z",
    );
    let higher_priority = task(
        "task-higher-priority",
        Status::Ready,
        0,
        10,
        "2026-06-22T12:03:00Z",
    );
    let backlog = task(
        "task-backlog",
        Status::Backlog,
        0,
        0,
        "2026-06-22T12:04:00Z",
    );

    let columns = board_columns([
        &later_created,
        &earlier_created,
        &lower_order,
        &higher_priority,
        &backlog,
    ]);
    let ready = columns
        .iter()
        .find(|column| column.status == Status::Ready)
        .unwrap();
    let ids: Vec<&str> = ready.tasks.iter().map(|task| task.id.as_str()).collect();
    assert_eq!(
        ids,
        vec![
            "task-higher-priority",
            "task-lower-order",
            "task-earlier-created",
            "task-later-created"
        ]
    );

    let rendered = render_board([
        &later_created,
        &earlier_created,
        &lower_order,
        &higher_priority,
        &backlog,
    ]);
    assert!(rendered.find("BACKLOG").unwrap() < rendered.find("READY").unwrap());
}

#[test]
fn move_updates_status_and_preserves_body() {
    let tmp = TempDir::new().unwrap();
    let store = TaskStore::new(tmp.path());
    store.init(false).unwrap();

    run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "new",
        "Move me",
        "--id",
        "task-move-me",
    ])
    .unwrap();
    let created = store.get_task("task-move-me").unwrap();
    assert!(created.body.contains("## Acceptance"));
    let raw = fs::read_to_string(&created.path).unwrap();
    fs::write(
        &created.path,
        raw.replace(
            &format!("updated_at = \"{}\"", created.task.updated_at),
            "updated_at = \"2000-01-01T00:00:00Z\"",
        ),
    )
    .unwrap();
    let before = store.get_task("task-move-me").unwrap();
    let output = run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "move",
        "task-move-me",
        "ready",
    ])
    .unwrap();
    assert!(output.contains("Moved task-move-me to ready"));

    let after = store.get_task("task-move-me").unwrap();
    assert_eq!(after.task.status, Status::Ready);
    assert_eq!(after.body.trim_start(), before.body.trim_start());
    assert_ne!(after.task.updated_at, before.task.updated_at);
}

#[test]
fn ready_reports_dependency_and_policy_filters() {
    let tmp = TempDir::new().unwrap();
    let store = TaskStore::new(tmp.path());
    store.init(false).unwrap();

    write_task(
        &store,
        "task-done-dep",
        "Done dep",
        "done",
        &["safe-auto"],
        &[],
        true,
    );
    write_task(
        &store,
        "task-review-dep",
        "Review dep",
        "review",
        &["safe-auto"],
        &[],
        true,
    );
    write_task(
        &store,
        "task-eligible",
        "Eligible",
        "ready",
        &["safe-auto"],
        &["task-done-dep"],
        true,
    );
    write_task(
        &store,
        "task-waiting",
        "Waiting",
        "ready",
        &["safe-auto"],
        &["task-review-dep"],
        true,
    );
    write_task(
        &store,
        "task-missing-label",
        "Missing label",
        "ready",
        &[],
        &[],
        true,
    );
    write_task(
        &store,
        "task-dangerous",
        "Dangerous",
        "ready",
        &["safe-auto", "dangerous"],
        &[],
        true,
    );
    write_task(
        &store,
        "task-not-opted",
        "Not opted",
        "ready",
        &["safe-auto"],
        &[],
        false,
    );

    let output = run_with_args(["myque", "--root", tmp.path().to_str().unwrap(), "ready"]).unwrap();
    assert!(output.contains("task-eligible"));
    assert!(output.contains("task-waiting"));
    assert!(output.contains("waiting on task-review-dep (review)"));
    assert!(output.contains("task-missing-label"));
    assert!(output.contains("missing allowed label: safe-auto"));
    assert!(output.contains("task-dangerous"));
    assert!(output.contains("blocked label: dangerous"));
    assert!(output.contains("task-not-opted"));
    assert!(output.contains("task did not opt in to auto dispatch"));
}

#[test]
fn ready_json_reports_machine_readable_eligible_tasks() {
    let tmp = TempDir::new().unwrap();
    let store = TaskStore::new(tmp.path());
    store.init(false).unwrap();
    write_task(
        &store,
        "task-ready-json",
        "Ready JSON",
        "ready",
        &["safe-auto"],
        &[],
        true,
    );
    write_task(
        &store,
        "task-blocked-json",
        "Blocked JSON",
        "blocked",
        &["safe-auto"],
        &[],
        true,
    );

    let output = run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "ready",
        "--json",
    ])
    .unwrap();
    let parsed: serde_json::Value = serde_json::from_str(&output).unwrap();

    assert_eq!(parsed.as_array().unwrap().len(), 1);
    assert_eq!(parsed[0]["id"], "task-ready-json");
    assert_eq!(parsed[0]["status"], "ready");
    assert_eq!(parsed[0]["backend"], "noop");
    assert_eq!(parsed[0]["allowed_auto_dispatch"], true);
}

#[test]
fn dispatch_task_flag_only_runs_requested_task() {
    let tmp = TempDir::new().unwrap();
    let store = TaskStore::new(tmp.path());
    store.init(false).unwrap();
    write_task(
        &store,
        "task-first",
        "First",
        "ready",
        &["safe-auto"],
        &[],
        true,
    );
    write_task(
        &store,
        "task-second",
        "Second",
        "ready",
        &["safe-auto"],
        &[],
        true,
    );

    let output = run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "dispatch",
        "--task",
        "task-second",
    ])
    .unwrap();

    assert!(output.contains("task-second"));
    assert_eq!(
        store.get_task("task-first").unwrap().task.status,
        Status::Ready
    );
    assert_eq!(
        store.get_task("task-second").unwrap().task.status,
        Status::Running
    );
}
#[test]
fn dry_run_does_not_mutate_task_files_or_runs() {
    let tmp = TempDir::new().unwrap();
    let store = TaskStore::new(tmp.path());
    store.init(false).unwrap();
    write_task(
        &store,
        "task-dry-run",
        "Dry run",
        "ready",
        &["safe-auto"],
        &[],
        true,
    );
    let before = read_task_files(&store);

    let output = run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "dispatch",
        "--dry-run",
    ])
    .unwrap();

    assert!(output.contains("Dry run: no files changed."));
    assert!(output.contains("task-dry-run"));
    assert_eq!(before, read_task_files(&store));
    assert_eq!(fs::read_dir(store.runs_dir()).unwrap().count(), 0);
}

#[test]
fn noop_dispatch_marks_running_and_writes_run_record() {
    let tmp = TempDir::new().unwrap();
    let store = TaskStore::new(tmp.path());
    store.init(false).unwrap();
    write_task(
        &store,
        "task-noop",
        "Noop",
        "ready",
        &["safe-auto"],
        &[],
        true,
    );

    let output =
        run_with_args(["myque", "--root", tmp.path().to_str().unwrap(), "dispatch"]).unwrap();
    assert!(output.contains("Started:"));
    assert!(output.contains("task-noop"));

    let stored = store.get_task("task-noop").unwrap();
    assert_eq!(stored.task.status, Status::Running);
    assert_eq!(stored.task.attempts, 1);
    let run_id = stored.task.last_run_id.as_deref().expect("last_run_id");
    assert!(stored.task.assigned_at.is_some());

    let run_path = store.runs_dir().join(format!("{run_id}.toml"));
    let run_record = fs::read_to_string(run_path).unwrap();
    assert!(run_record.contains(&format!("id = \"{run_id}\"")));
    assert!(run_record.contains("task_id = \"task-noop\""));
    assert!(run_record.contains("backend = \"noop\""));
    assert!(run_record.contains("status = \"started\""));
}

#[test]
fn edit_updates_frontmatter_and_preserves_body() {
    let tmp = TempDir::new().unwrap();
    let store = TaskStore::new(tmp.path());
    store.init(false).unwrap();
    write_task(&store, "task-edit", "Edit", "backlog", &[], &[], false);
    let before = store.get_task("task-edit").unwrap();

    let output = run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "edit",
        "task-edit",
        "--title",
        "Edited title",
        "--priority",
        "5",
        "--allow-auto",
    ])
    .unwrap();

    assert_eq!(output, "Updated task-edit\n");
    let after = store.get_task("task-edit").unwrap();
    assert_eq!(after.task.title, "Edited title");
    assert_eq!(after.task.priority, 5);
    assert!(after.task.allowed_auto_dispatch);
    assert_eq!(after.body, before.body);
    assert_ne!(after.task.updated_at, before.task.updated_at);
}

#[test]
fn label_add_remove_is_idempotent_and_dedupes() {
    let tmp = TempDir::new().unwrap();
    let store = TaskStore::new(tmp.path());
    store.init(false).unwrap();
    write_task(
        &store,
        "task-label",
        "Label",
        "backlog",
        &["old", "safe-auto"],
        &[],
        false,
    );

    run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "label",
        "task-label",
        "--add",
        "safe-auto",
        "--add",
        "cli",
        "--remove",
        "old",
    ])
    .unwrap();

    let after = store.get_task("task-label").unwrap();
    assert_eq!(after.task.labels, vec!["safe-auto", "cli"]);
}

#[test]
fn deps_rejects_unknown_self_and_cycle() {
    let tmp = TempDir::new().unwrap();
    let store = TaskStore::new(tmp.path());
    store.init(false).unwrap();
    write_task(&store, "task-a", "A", "backlog", &[], &[], false);
    write_task(&store, "task-b", "B", "backlog", &[], &["task-a"], false);

    let unknown = run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "deps",
        "task-a",
        "--add",
        "missing",
    ])
    .unwrap_err()
    .to_string();
    assert!(unknown.contains("validation failed"));
    assert_eq!(
        store.get_task("task-a").unwrap().task.depends_on,
        Vec::<String>::new()
    );

    let self_dep = run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "deps",
        "task-a",
        "--add",
        "task-a",
    ])
    .unwrap_err()
    .to_string();
    assert!(self_dep.contains("validation failed"));
    assert_eq!(
        store.get_task("task-a").unwrap().task.depends_on,
        Vec::<String>::new()
    );

    let cycle = run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "deps",
        "task-a",
        "--add",
        "task-b",
    ])
    .unwrap_err()
    .to_string();
    assert!(cycle.contains("validation failed"));
    assert_eq!(
        store.get_task("task-a").unwrap().task.depends_on,
        Vec::<String>::new()
    );
}

#[test]
fn section_replaces_only_target_section_and_appends_optional_section() {
    let tmp = TempDir::new().unwrap();
    let store = TaskStore::new(tmp.path());
    store.init(false).unwrap();
    write_task(
        &store,
        "task-section",
        "Section",
        "backlog",
        &[],
        &[],
        false,
    );

    run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "section",
        "task-section",
        "acceptance",
        "- New acceptance",
    ])
    .unwrap();
    run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "section",
        "task-section",
        "files",
        "- `src/cli.rs`",
        "--append",
    ])
    .unwrap();

    let after = store.get_task("task-section").unwrap();
    assert!(after.body.contains("## Goal\n\nFinish Section."));
    assert!(after.body.contains("## Acceptance\n\n- New acceptance"));
    assert!(after.body.contains("## Files\n\n- `src/cli.rs`"));
    assert!(!after.body.contains("- Observable result exists."));
}

#[test]
fn fail_and_complete_update_status_metadata_and_preserve_body() {
    let tmp = TempDir::new().unwrap();
    let store = TaskStore::new(tmp.path());
    store.init(false).unwrap();
    write_task(&store, "task-fail", "Fail", "running", &[], &[], false);
    write_task(
        &store,
        "task-complete",
        "Complete",
        "running",
        &[],
        &[],
        false,
    );
    let before_fail = store.get_task("task-fail").unwrap();
    let before_complete = store.get_task("task-complete").unwrap();

    run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "fail",
        "task-fail",
        "--reason",
        "test failure",
    ])
    .unwrap();
    run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "complete",
        "task-complete",
    ])
    .unwrap();

    let failed = store.get_task("task-fail").unwrap();
    assert_eq!(failed.task.status, Status::Failed);
    assert_eq!(failed.task.failure_reason.as_deref(), Some("test failure"));
    assert!(failed.task.completed_at.is_some());
    assert_eq!(failed.body, before_fail.body);

    let completed = store.get_task("task-complete").unwrap();
    assert_eq!(completed.task.status, Status::Review);
    assert!(completed.task.completed_at.is_some());
    assert_eq!(completed.body, before_complete.body);
}

#[test]
fn complete_done_rejected_unless_policy_allows_agents_may_mark_done() {
    let tmp = TempDir::new().unwrap();
    let store = TaskStore::new(tmp.path());
    store.init(false).unwrap();
    write_task(&store, "task-done", "Done", "running", &[], &[], false);

    let rejected = run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "complete",
        "task-done",
        "--done",
    ])
    .unwrap_err()
    .to_string();
    assert!(rejected.contains("validation failed"));
    assert_eq!(
        store.get_task("task-done").unwrap().task.status,
        Status::Running
    );

    let mut config = Config::default();
    config.policy.agents_may_mark_done = true;
    store.write_config(&config).unwrap();
    run_with_args([
        "myque",
        "--root",
        tmp.path().to_str().unwrap(),
        "complete",
        "task-done",
        "--done",
    ])
    .unwrap();
    assert_eq!(
        store.get_task("task-done").unwrap().task.status,
        Status::Done
    );
}
