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
