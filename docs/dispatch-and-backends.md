# Dispatch and backends

Dispatch is deliberately conservative. A task can run only when task metadata, dependency state, config policy, and backend capability all allow it.

## Eligibility checks

A task is eligible only when every check passes:

1. `status == "ready"`.
2. `allowed_auto_dispatch == true` when config requires task-level opt-in.
3. The task has at least one label from `policy.allowed_labels` when config requires an allowed label.
4. The task has no label from `policy.blocked_labels`.
5. `attempts < max_attempts`.
6. Every ID in `depends_on` exists.
7. Every dependency has `status == "done"`.
8. Required Markdown sections are present.
9. `## Acceptance` is non-empty when policy requires it.
10. The requested `agent` exists in config.
11. The requested backend exists.
12. The backend `can_run` check allows the task.
13. The global concurrency limit allows another run.

Dry-run output explains both eligible and skipped tasks.

```txt
Eligible:
  task-2026-06-22-001  Add retry policy       agent=coder backend=noop

Skipped:
  task-2026-06-22-002  Delete old cache       blocked label: destructive
  task-2026-06-22-003  Update docs            missing allowed label: safe-auto
  task-2026-06-22-004  Refactor parser        waiting on task-2026-06-21-002
```

## Dry-run dispatch

`myque dispatch --dry-run`:

1. Loads config.
2. Loads task files.
3. Validates task schema.
4. Builds the dependency graph.
5. Computes eligible and skipped tasks.
6. Asks each backend whether it can run the task.
7. Prints the dispatch plan.
8. Does not mutate files.

## Real dispatch

`myque dispatch` runs the same planning algorithm, then selects tasks up to `max_parallel` minus the current running count.

For each accepted task, MyQue:

1. Calls the backend `dispatch(task, config, run_id)` operation.
2. Increments `attempts`.
3. Sets `last_run_id` and `assigned_at`.
4. Chooses the task status from the dispatch result:
   - no `ended_at` returned → the run is still in flight → `running`;
   - `ended_at` with `exit_code == 0` → `done`;
   - `ended_at` with any other or missing exit code → `failed`.
5. Sets `completed_at` and `updated_at` from `ended_at` for terminal runs.
6. Writes a run record under `.myque/runs/` whose `status` mirrors the task
   status (`started`, `done`, or `failed`).

A backend that runs the task to completion reports `ended_at`/`exit_code`, so the
task lands directly in `done`/`failed`; the caller-supplied `chilin` backend works
this way. An asynchronous backend that only starts work returns no `ended_at`,
leaving the task `running` until a later status update.

If a backend rejects a task before start (`started == false`), MyQue does not mark
it `running`, increment `attempts`, or write a run record. The reason is reported
in command output.

## Run records

Run metadata is stored separately from task cards.

```txt
.myque/runs/run-2026-06-22-001.toml
```

Example:

```toml
id = "run-2026-06-22-001"
task_id = "task-2026-06-22-001"
backend = "noop"
agent = "coder"
status = "started"
started_at = "2026-06-22T12:10:00Z"
ended_at = ""
message = "noop backend accepted task"
```

A backend that finishes synchronously records a terminal run, including the exit
code:

```toml
id = "run-2026-06-22-002"
task_id = "task-2026-06-22-002"
backend = "chilin"
agent = "ci"
status = "failed"
started_at = "2026-06-22T12:11:00Z"
ended_at = "2026-06-22T12:11:03Z"
message = "chilin command failed with exit 1; log=data/ci/logs/owner/repo/run-2.log"
exit_code = 1
```

Task files track current state. Run records track execution history.

## Backend contract

Backends expose capability checks, dispatch, status, and cancel operations. The Rust implementation represents this as a trait; equivalent implementations in other languages should keep the same responsibilities.

```ts
type BackendDecision = {
  allowed: boolean;
  reason?: string;
};

type DispatchResult = {
  runId: string;
  started: boolean;
  message: string;
  // Present when the backend ran the task to completion. MyQue uses these to
  // mark the task done/failed; absent means the run is still in flight.
  endedAt?: string;
  exitCode?: number;
};

type RunStatus = {
  runId: string;
  status: "running" | "review" | "done" | "failed" | "cancelled" | "unknown";
  message?: string;
};

interface AgentBackend {
  name: string;
  canRun(task: TaskCard, config: Config): Promise<BackendDecision>;
  // MyQue generates the run id and passes it in; the backend reports the outcome.
  dispatch(task: TaskCard, config: Config, runId: string): Promise<DispatchResult>;
  status(runId: string, config: Config): Promise<RunStatus>;
  cancel(runId: string, config: Config): Promise<void>;
}
```

Backends report runtime capability. They do not decide task eligibility; eligibility belongs to MyQue's policy and dependency engine.

## `noop` backend

The `noop` backend validates lifecycle behavior without starting a real agent.

Behavior:

- `can_run`: allows tasks unless config forbids `noop`.
- `dispatch`: returns a generated run ID and `started = true`.
- `status`: reports a static running-style status.
- `cancel`: can mark run records cancelled when mutable run records are supported.

## `shell` backend

The `shell` backend bridges to local agent CLIs.

```toml
[agents.coder]
backend = "shell"
command = "codex run --task-file {task_file}"
```

Rules:

- Replace only known placeholders such as `{task_file}`, `{task_id}`, and `{workspace}`.
- Do not invoke shell commands through unescaped string concatenation.
- Capture command, exit code, start/end timestamps, and stdout/stderr paths or summaries.
- A successful command reports `started = true` with `ended_at`/`exit_code`, so MyQue marks the task `done`.
- A nonzero exit or a failed process start reports `started = false`, so MyQue rejects the dispatch and leaves the task `ready` (attempts unchanged).

## Caller-injectable backends

MyQue ships only the `noop` and `shell` backends, but an embedding program can register its own `AgentBackend` implementation without MyQue depending on it.

`BackendRegistry` owns the built-in backends plus any caller-registered ones, keyed by `AgentBackend::name()`. `dispatch_with` runs the normal dispatch algorithm against a supplied registry; `dispatch` is a thin wrapper that uses a builtins-only registry.

```rust
let mut registry = myque::BackendRegistry::with_builtins();
registry.register(Box::new(chilin::ChilinRunner::new(runner, log_dir))); // name() -> "chilin"
let outcome = myque::dispatch_with(&store, &config, false, &registry)?;
```

- A registered backend whose `name()` matches an existing entry (e.g. `shell`) replaces it.
- A task routes to a backend by its agent config: `[agents.<name>] backend = "chilin"` resolves to the `"chilin"` backend.
- Eligibility still requires the resolved backend to be `noop`/`shell` or present in `[backends.<name>]`. If eligibility selects a task whose backend is not registered, `dispatch_with` returns `BackendError::UnknownBackend` and aborts the run.
- Dispatch is synchronous, single-threaded filesystem work. From async code call `dispatch_with` inside `tokio::task::spawn_blocking`.
