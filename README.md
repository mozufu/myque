# myque

MyQue (`/maɪk/`, "my queue") is a local, Git-native task queue for autonomous agents.

It stores one Markdown file per task, renders those files as a Kanban-style board, validates dependency and dispatch policy, and records backend runs under the repository. The goal is Trello-like visibility for humans with deterministic, inspectable queue semantics for agents.

## What it does

- Creates and manages `.myque/` task stores.
- Uses human-editable Markdown task files with TOML frontmatter.
- Lists, shows, moves, validates, and renders tasks as a board.
- Computes which tasks are ready for automated dispatch.
- Guards dispatch with task-level opt-in, label policy, dependency checks, and attempt limits.
- Runs through backend adapters, currently including `noop` and `shell`.
- Records run metadata separately from task cards.

## Install and build

MyQue uses the Rust 2024 edition, so building from source requires Rust 1.85 or newer.

Use the Nix flake for a reproducible development shell:

```sh
nix develop
cargo build
cargo test
```

Run one command inside the flake shell without entering it:

```sh
nix develop --command cargo test
```

Run the CLI directly during development:

```sh
cargo run -- --help
cargo run -- init
```

Install locally with Cargo:

```sh
cargo install --path .
```

## Quick start

Initialize a queue in the current repository:

```sh
myque init
```

Create tasks:

```sh
myque new "Draft queue docs"
myque new "Wire shell backend" --status ready --label safe-auto --agent coder --allow-auto
myque new "Review backend output" --depends-on task-2026-06-22-001
```

Inspect the queue:

```sh
myque list
myque board
myque show task-2026-06-22-001
```

Move a task through the lifecycle:

```sh
myque move task-2026-06-22-001 ready
myque move task-2026-06-22-001 review
```

Validate all task files and config:

```sh
myque validate
```

Preview dispatch without mutations:

```sh
myque ready
myque dispatch --dry-run
```

Dispatch eligible tasks:

```sh
myque dispatch
```

## Task storage

MyQue stores state under `.myque/`:

```txt
.myque/
  config.toml
  tasks/
    task-2026-06-22-001.md
  runs/
    run-2026-06-22-001.toml
```

Task status is frontmatter, not a directory name. Stable task paths keep dependency links, editor tabs, Git history, and external tools from breaking when a card moves.

A task file looks like this:

```md
+++
id = "task-2026-06-22-001"
title = "Add retry policy"
status = "ready"
priority = 2
order = 1000
labels = ["backend", "safe-auto"]
agent = "coder"
backend = "noop"
depends_on = []
allowed_auto_dispatch = true
attempts = 0
max_attempts = 2
created_at = "2026-06-22T12:00:00Z"
updated_at = "2026-06-22T12:00:00Z"
+++

## Goal

Add retry policy to the sync worker.

## Context

Current worker fails permanently on transient network errors.

## Constraints

Do not change the auth storage format.

## Acceptance

- Retries transient errors up to 3 times.
- Does not retry validation errors.
```

## Lifecycle

Allowed statuses:

```txt
backlog -> ready -> running -> review -> done
             |          |
             v          v
          blocked     failed
             |          |
             v          v
           ready      ready
```

Additional terminal status: `cancelled`.

Rules:

- `ready` tasks can dispatch only when policy allows them.
- Agents should move completed work to `review`, not `done`, unless policy explicitly allows direct completion.
- Only `done` dependencies unblock downstream tasks by default.
- `review` is not equivalent to `done`.

## Dispatch safety model

A task is eligible for dispatch only when all relevant checks pass:

- Status is `ready`.
- Task opt-in satisfies `allowed_auto_dispatch` policy.
- Labels satisfy `policy.allowed_labels` and avoid `policy.blocked_labels`.
- `attempts < max_attempts`.
- Dependencies exist and are `done`.
- Required Markdown sections exist.
- `## Acceptance` is non-empty when policy requires it.
- The requested agent and backend exist.
- Backend capability check allows the task.
- Global concurrency has capacity.

`myque dispatch --dry-run` reports eligible and skipped tasks without mutating files.

## Documentation

- [Documentation index](docs/README.md)
- [Task storage and format](docs/task-format.md)
- [CLI usage](docs/cli.md)
- [Dispatch and backends](docs/dispatch-and-backends.md)
- [Development notes](docs/development.md)

## Non-goals for v1

- Cloud sync.
- Heavy web UI or drag-and-drop UI.
- Multi-user permissions.
- Continuous daemon mode.
- Autonomous decomposition of vague milestones.
