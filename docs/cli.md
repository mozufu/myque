# CLI usage

All commands accept `--root <path>` to point at a repository root. The default root is the current directory.

```sh
myque --root /path/to/repo board
```

## `myque init`

Creates the local queue layout:

```txt
.myque/config.toml
.myque/tasks/
.myque/runs/
```

Use `--force` to replace an existing config with defaults.

```sh
myque init
myque init --force
```

## `myque new "Title"`

Creates a task Markdown file under `.myque/tasks/`.

```sh
myque new "Add retry policy"
myque new "Fix parser" --status ready --label safe-auto --agent coder
myque new "Write tests" --depends-on task-2026-06-22-001
```

Useful options:

| Option | Purpose |
| --- | --- |
| `--id <id>` | Use an explicit task ID. |
| `--status <status>` | Set initial lifecycle status. Defaults to `backlog`. |
| `--priority <n>` | Sort priority. Lower values come first. Defaults to `2`. |
| `--order <n>` | Manual order inside a status column. Defaults to `1000`. |
| `--label <label>` | Add a label. Repeat for multiple labels. |
| `--agent <agent>` | Set the logical agent. |
| `--backend <backend>` | Set the backend adapter. |
| `--depends-on <task-id>` | Add a dependency. Repeat for multiple dependencies. |
| `--allowed-auto-dispatch` / `--allow-auto` | Opt the task into automatic dispatch. |
| `--max-attempts <n>` | Override the dispatch attempt limit. |

## `myque list`

Lists tasks compactly.

```sh
myque list
myque list --status ready
myque list --label safe-auto
myque list --agent coder
```

Filters can be combined.

## `myque show <task-id>`

Prints one task's metadata and Markdown body.

```sh
myque show task-2026-06-22-001
```

## `myque board`

Renders task cards by lifecycle status.

```sh
myque board
myque board --grouped
```

Within each status group, cards sort by:

1. `priority` ascending.
2. `order` ascending.
3. `created_at` ascending.

`board` is read-only and never mutates task files.

## `myque move <task-id> <status>`

Updates task status and `updated_at`.

```sh
myque move task-2026-06-22-001 ready
myque move task-2026-06-22-001 review
```

Unknown statuses are rejected.

## `myque edit <task-id>`

Patches frontmatter fields while preserving the Markdown body.

```sh
myque edit task-2026-06-22-001 --title "Add retry policy" --priority 1
myque edit task-2026-06-22-001 --agent coder --backend shell --max-attempts 3
myque edit task-2026-06-22-001 --allow-auto
myque edit task-2026-06-22-001 --no-allow-auto
```

## `myque label <task-id>`

Adds or removes labels idempotently.

```sh
myque label task-2026-06-22-001 --add safe-auto --add backend
myque label task-2026-06-22-001 --remove needs-human
```

## `myque deps <task-id>`

Adds or removes dependencies idempotently. The resulting dependency graph must reference existing tasks and remain acyclic.

```sh
myque deps task-2026-06-22-001 --add task-2026-06-22-000
myque deps task-2026-06-22-001 --remove task-2026-06-21-999
```

## `myque section <task-id> <section>`

Replaces or appends one Markdown section. Supported sections are `goal`, `context`, `constraints`, `acceptance`, `files`, and `notes`.

```sh
myque section task-2026-06-22-001 acceptance "- Retries transient errors."
myque section task-2026-06-22-001 files --append '- `src/worker.rs`'
printf '%s\n' '- New acceptance item' | myque section task-2026-06-22-001 acceptance --stdin
```

## `myque fail <task-id>` and `myque complete <task-id>`

Worker-friendly lifecycle shortcuts.

```sh
myque fail task-2026-06-22-001 --reason "tests failed"
myque complete task-2026-06-22-001
```

`complete` moves tasks to `review` by default. `complete --done` is rejected unless `policy.agents_may_mark_done = true`.

## `myque validate`

Validates all task files and config.

Checks include:

- TOML frontmatter parses.
- Required fields exist.
- Required Markdown sections exist where policy requires them.
- Task IDs are unique.
- Dependencies point to existing tasks.
- Dependency graph is acyclic.
- Status values are valid.
- Agent and backend references exist.

```sh
myque validate
```

## `myque ready`

Prints eligibility without starting a backend or mutating files.

```sh
myque ready
```

## `myque dispatch --dry-run`

Prints eligible and skipped tasks, including skip reasons. Does not mutate files.

```sh
myque dispatch --dry-run
```

## `myque dispatch`

Dispatches eligible tasks through configured backends. For each accepted dispatch, MyQue increments attempts, marks the task `running`, assigns a run ID, updates timestamps, and writes a run record under `.myque/runs/`.

```sh
myque dispatch
```
