# AGENTS.md

Guidance for agents working in this repository.

## Project shape

MyQue is a Rust library and CLI for a local, Git-native task queue. It stores queue state in `.myque/` using Markdown task files with TOML frontmatter, renders those files as a board, validates dispatch safety, and records backend runs.

Important paths:

- `Cargo.toml` — crate manifest for the `myque` library and binary.
- `src/model.rs` — task, config, status, policy, and backend config types.
- `src/frontmatter.rs` — TOML frontmatter and Markdown body parsing/writing.
- `src/validation.rs` — task/config/dependency validation.
- `src/store.rs` — `.myque/` filesystem store operations.
- `src/eligibility.rs` — dispatch eligibility and skip reasons.
- `src/backend.rs` — backend trait, `noop`, `shell`, dispatch, and run records.
- `src/board.rs` — board grouping/rendering.
- `src/cli.rs` — CLI argument model and command execution.
- `tests/cli_board.rs` — behavior-focused CLI and board tests.
- `docs/` — user and maintainer documentation.

## Commands

Use the Nix flake for the development environment:

```sh
nix develop
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo check --bin myque
cargo test
```

For one-off commands without entering a shell:

```sh
nix develop --command cargo test
```

The flake sets Darwin SDK linker variables needed for local validation on macOS. If Nix is unavailable, use the fallback environment explicitly:

```sh
LIBRARY_PATH=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib cargo test
```

For a narrow code change, run the smallest command that covers it first, then run the relevant full check before finishing. For doc-only changes, `cargo test` is enough to prove the checked-in code still passes unless the docs describe changed behavior.

## Task model invariants

Preserve these invariants:

- Every task ID is unique.
- Every dependency target exists.
- Dependency graphs are acyclic.
- Task status is frontmatter, not a path segment.
- `board` never mutates task files.
- `dispatch --dry-run` never mutates task files.
- Real dispatch mutates task state only after a backend accepts the task.
- Auto-dispatch requires task-level opt-in and policy-level opt-in.
- `review` is not equivalent to `done`.
- Only `done` dependencies unblock downstream tasks by default.
- Backend adapters report runtime capability; they do not decide eligibility policy.

## CLI behavior contracts

- `myque init` creates `.myque/config.toml`, `.myque/tasks/`, and `.myque/runs/`.
- `myque new` creates one Markdown task with required frontmatter and sections.
- `myque list`, `show`, `board`, `validate`, `ready`, and `dispatch --dry-run` are read-only.
- `myque move` updates only status-related metadata and preserves Markdown body text.
- `myque dispatch` increments attempts exactly once for an accepted dispatch, marks the task `running`, writes timestamps, sets `last_run_id`, and writes a run record.

## Dispatch safety

A task can dispatch only when it passes policy, dependency, schema, backend, and concurrency checks. Keep skip reasons actionable: users and agents should be able to read the output and know what to change.

Required safety checks include:

- `status == "ready"`.
- `allowed_auto_dispatch == true` when policy requires it.
- Required allowed labels are present.
- Blocked labels are absent.
- Attempts are below the task/config limit.
- Dependencies exist and are `done`.
- Required Markdown sections exist.
- `## Acceptance` is non-empty when policy requires it.
- Agent and backend references exist.
- Backend `can_run` allows execution.
- Global concurrency permits another run.

## Documentation expectations

Keep `README.md` user-facing: what MyQue is, how to build it, quick start, and links to deeper docs.

Keep `AGENTS.md` agent-facing: repo layout, commands, invariants, and change contracts. Do not duplicate every API detail here; link or point to `docs/` when adding long-form reference material.

When changing CLI behavior, update both:

- `README.md` if users need to know about it.
- `docs/cli.md` and any affected reference docs.

When changing task schema, policy, lifecycle, or backend behavior, update the matching page under `docs/` and adjust this file if an invariant changes.
