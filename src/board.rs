use crate::model::{Status, Task};

const STATUS_COLUMNS: &[Status] = &[
    Status::Backlog,
    Status::Ready,
    Status::Blocked,
    Status::Running,
    Status::Review,
    Status::Done,
    Status::Failed,
    Status::Cancelled,
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BoardStyle {
    #[default]
    Columns,
    Grouped,
}

#[derive(Debug, Clone)]
pub struct BoardOptions {
    pub style: BoardStyle,
}

impl Default for BoardOptions {
    fn default() -> Self {
        Self {
            style: BoardStyle::Columns,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BoardColumn<'a> {
    pub status: Status,
    pub tasks: Vec<&'a Task>,
}

pub fn board_columns<'a>(tasks: impl IntoIterator<Item = &'a Task>) -> Vec<BoardColumn<'a>> {
    let mut columns: Vec<BoardColumn<'a>> = STATUS_COLUMNS
        .iter()
        .cloned()
        .map(|status| BoardColumn {
            status,
            tasks: Vec::new(),
        })
        .collect();

    for task in tasks {
        if let Some(column) = columns
            .iter_mut()
            .find(|column| column.status == task.status)
        {
            column.tasks.push(task);
        }
    }

    for column in &mut columns {
        sort_tasks(&mut column.tasks);
    }

    columns
}

pub fn sorted_tasks<'a>(tasks: impl IntoIterator<Item = &'a Task>) -> Vec<&'a Task> {
    let mut tasks: Vec<&Task> = tasks.into_iter().collect();
    tasks.sort_by(|left, right| {
        status_rank(&left.status)
            .cmp(&status_rank(&right.status))
            .then(left.priority.cmp(&right.priority))
            .then(left.order.cmp(&right.order))
            .then(left.created_at.cmp(&right.created_at))
            .then(left.id.cmp(&right.id))
    });
    tasks
}

pub fn render_board<'a>(tasks: impl IntoIterator<Item = &'a Task>) -> String {
    render_board_with_options(tasks, &BoardOptions::default())
}

pub fn render_board_with_options<'a>(
    tasks: impl IntoIterator<Item = &'a Task>,
    options: &BoardOptions,
) -> String {
    let columns = board_columns(tasks);
    match options.style {
        BoardStyle::Columns => render_columns(&columns),
        BoardStyle::Grouped => render_grouped(&columns),
    }
}

pub fn render_task_list<'a>(tasks: impl IntoIterator<Item = &'a Task>) -> String {
    let mut out = String::new();
    for task in sorted_tasks(tasks) {
        out.push_str(&format!(
            "{:<26} {:<9} p={:<3} o={:<5} {}\n",
            task.id, task.status, task.priority, task.order, task.title
        ));
    }
    out
}

fn render_columns(columns: &[BoardColumn<'_>]) -> String {
    let visible: Vec<&BoardColumn<'_>> = columns
        .iter()
        .filter(|column| {
            !column.tasks.is_empty()
                || matches!(
                    column.status,
                    Status::Backlog
                        | Status::Ready
                        | Status::Running
                        | Status::Review
                        | Status::Done
                )
        })
        .collect();
    if visible.is_empty() {
        return "No tasks.\n".to_owned();
    }

    let mut widths: Vec<usize> = visible
        .iter()
        .map(|column| column.status.as_str().len().max(20))
        .collect();
    for (index, column) in visible.iter().enumerate() {
        for task in &column.tasks {
            widths[index] = widths[index].max(card_text(task).len());
        }
    }

    let mut out = String::new();
    for (index, column) in visible.iter().enumerate() {
        if index > 0 {
            out.push_str("  ");
        }
        out.push_str(&format!(
            "{:<width$}",
            column.status.as_str().to_uppercase(),
            width = widths[index]
        ));
    }
    out.push('\n');

    let max_rows = visible
        .iter()
        .map(|column| column.tasks.len())
        .max()
        .unwrap_or(0);
    for row in 0..max_rows {
        for (index, column) in visible.iter().enumerate() {
            if index > 0 {
                out.push_str("  ");
            }
            let cell = column
                .tasks
                .get(row)
                .map(|task| card_text(task))
                .unwrap_or_default();
            out.push_str(&format!("{:<width$}", cell, width = widths[index]));
        }
        out.push('\n');
    }

    out
}

fn render_grouped(columns: &[BoardColumn<'_>]) -> String {
    let mut out = String::new();
    for column in columns {
        if column.tasks.is_empty() {
            continue;
        }
        out.push_str(&column.status.as_str().to_uppercase());
        out.push('\n');
        for task in &column.tasks {
            out.push_str(&format!(
                "  {:<26} p={:<3} o={:<5} {}\n",
                task.id, task.priority, task.order, task.title
            ));
        }
    }
    if out.is_empty() {
        out.push_str("No tasks.\n");
    }
    out
}

fn sort_tasks(tasks: &mut [&Task]) {
    tasks.sort_by(|left, right| {
        left.priority
            .cmp(&right.priority)
            .then(left.order.cmp(&right.order))
            .then(left.created_at.cmp(&right.created_at))
            .then(left.id.cmp(&right.id))
    });
}

fn card_text(task: &Task) -> String {
    format!("{} {}", task.id, task.title)
}

fn status_rank(status: &Status) -> usize {
    STATUS_COLUMNS
        .iter()
        .position(|candidate| candidate == status)
        .unwrap_or(usize::MAX)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{Status, Task, TaskSections};

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

    #[test]
    fn groups_and_sorts_cards() {
        let newer = task("task-newer", Status::Ready, 1, 10, "2026-06-22T12:01:00Z");
        let older = task("task-older", Status::Ready, 1, 10, "2026-06-22T12:00:00Z");
        let first = task("task-first", Status::Ready, 1, 5, "2026-06-22T12:02:00Z");
        let backlog = task(
            "task-backlog",
            Status::Backlog,
            0,
            0,
            "2026-06-22T11:00:00Z",
        );
        let columns = board_columns([&newer, &older, &first, &backlog]);
        let ready = columns
            .iter()
            .find(|column| column.status == Status::Ready)
            .expect("ready column");
        let ids: Vec<&str> = ready.tasks.iter().map(|task| task.id.as_str()).collect();
        assert_eq!(ids, vec!["task-first", "task-older", "task-newer"]);
    }
}
