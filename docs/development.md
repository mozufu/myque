# Development notes

MyQue is a local, file-based queue. The implementation favors boring, inspectable behavior over hidden automation.

## Core invariants

- Every task ID is unique.
- Every dependency target exists.
- The dependency graph has no cycles.
- `board` rendering never mutates files.
- `dispatch --dry-run` never mutates files.
- Real dispatch mutates task state only after a backend accepts the task.
- Auto-dispatch requires both task opt-in and policy opt-in.
- `review` is not equivalent to `done`.
- Backend adapters do not decide task eligibility; they only report runtime capability.

## Validation errors

Validation should be structured enough for CLI output and future integrations to explain exact failures.

Representative categories:

```txt
config.invalid_toml
task.invalid_toml
task.missing_required_field
task.invalid_status
task.duplicate_id
task.missing_section
task.empty_acceptance
deps.unknown_task
deps.cycle
policy.missing_allowed_label
policy.blocked_label
backend.unknown_backend
agent.unknown_agent
```

## Verification coverage

Behavior-focused tests should cover:

1. Parsing TOML frontmatter and Markdown body.
2. Rejecting malformed TOML.
3. Rejecting missing required frontmatter fields.
4. Rejecting duplicate task IDs.
5. Rejecting unknown dependency IDs.
6. Detecting dependency cycles.
7. Treating `done` dependencies as unblocked.
8. Treating `review` dependencies as still blocked.
9. Requiring the `safe-auto` label when policy requires it.
10. Rejecting blocked labels such as `dangerous`.
11. Requiring non-empty `## Acceptance` for auto-dispatch.
12. Ensuring `dispatch --dry-run` does not change task files.
13. Ensuring `dispatch` with `noop` marks tasks `running`.
14. Ensuring `dispatch` with `noop` writes run records.
15. Grouping board output by status.
16. Sorting board output by priority, order, then creation time.
17. Moving a task updates `status` and `updated_at` without deleting body text.
18. Editing task frontmatter through the CLI preserves Markdown body text.
19. Label and dependency CLI mutations are idempotent and validate dependencies.
20. Section CLI mutations replace only the selected Markdown section.
21. Worker completion commands preserve body text and enforce `agents_may_mark_done`.

## Design risks

### The CLI grows into a project management app

Keep v1 commands minimal. Prioritize eligibility, policy, and backend abstractions over UI polish.

### Auto-dispatch runs unsafe tasks

Require task-level `allowed_auto_dispatch = true`, allowlisted labels, no blocked labels, and complete acceptance criteria.

### Agents receive under-specified tasks

Validate required Markdown sections before dispatch.

### Backend details leak into task metadata

Keep backend metadata minimal in task frontmatter. Store run-specific data in `.myque/runs`.

### Manual edits corrupt task files

Parsing should fail loudly with exact file and error context. Writers should preserve Markdown bodies and rewrite only frontmatter when possible.
