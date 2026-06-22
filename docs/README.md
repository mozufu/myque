# MyQue documentation

MyQue is a local, Git-native task queue for autonomous agents. It stores one Markdown file per task, renders the queue as a Kanban-style board, validates dispatch safety rules, and records backend runs without requiring cloud sync or a daemon.

## Documentation map

- [Task storage and format](task-format.md): `.myque/` layout, task frontmatter, Markdown sections, lifecycle statuses, and config.
- [CLI usage](cli.md): commands for creating, viewing, moving, validating, and dispatching tasks.
- [Dispatch and backends](dispatch-and-backends.md): eligibility checks, dry-run behavior, run records, and backend contracts.
- [Development notes](development.md): invariants, verification coverage, and design constraints.

## Design goals

- Local files are the source of truth.
- Task files stay pleasant to edit and review manually.
- Automation decisions are explainable from task files and `.myque/config.toml`.
- Dispatch requires explicit task-level and policy-level opt-in.
- Backend-specific execution details stay outside the core task model.

## Non-goals for v1

- Cloud sync.
- Heavy web UI or drag-and-drop UI.
- Multi-user permissions.
- Continuous daemon mode.
- Autonomous decomposition of vague milestones.
