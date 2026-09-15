# MyQue 0.2 storage and machine contract

## Work item envelope

New items use `work-item/v2`. `work-item/v1` remains supported as a closed
record; unknown versions and unknown v1 fields are errors. Unsupported clients
must refuse v2 and must never downgrade it. Canonical identity is MyQue-allocated
UUIDv7, independent of human `key`. Active files are `<storage.items>/<UUID>.md`.
The frontmatter and first level-one title remain the envelope. Everything after
the title's line terminator is opaque UTF-8 Markdown: CRLF, trailing whitespace,
fences and absence of a final newline survive every write. `bodyAfterTitle` and
`setBodyAfterTitle` are the supported library accessors; legacy `itemBody`
includes the title. `show` emits that body without trimming it.

The fixed owned keys for v2 are `schema`, `id`, `key`, `kind`, `state`, `created`,
`updated`, `closed`, `tags`, `parent`, `depends`, `blocks`, `related`,
`duplicate_of`, `supersedes`. Their existing types and meanings are unchanged:
required schema/id/kind/state/created scalars; optional key/updated/closed/parent/
duplicate_of scalars; remaining fields scalar lists. UUID references are
canonical UUIDs, timestamps are ISO-8601, and terminal states require `closed`.
Adding or repurposing an owned key requires a new envelope version.

All other unquoted top-level namespace names (`[a-z0-9_-]+`) are consumer owned.
Use one namespace per consumer, such as `devloop`. Reserved names cannot be
consumer namespaces. YAML must be a single well-formed mapping; duplicate,
complex or quoted top-level keys are rejected. Consumer values may contain
nested maps, arrays, comments and block scalars. MyQue validates syntax only and
retains complete raw entries, starting at `namespace:` and ending with the
newline before the next entry. Consumers must not duplicate envelope identity,
title, lifecycle/timestamps or relationships in their payloads. APIs refuse
namespace/key mismatch and raw entry injection. A consumer entry may be replaced
only by explicitly patching that namespace; unspecified namespaces survive.

States remain open, active, blocked, deferred, done, cancelled. `depends` plus
the inverse of `blocks` form dependency edges. Readiness means open with every
dependency done; cancelled never satisfies a dependency. Parent and dependency
graphs must be acyclic. UUID resolution, graph checks and readiness include
terminal records and do not fetch Git history.

## Supported machine API

`myque api get UUID` returns one JSON object with `api: "myque/v2"`, `id`,
`schema`, `state`, `title`, `body`, `consumers`, `revision`, `retired`, `ready`,
`dependenciesDone`, `bodyAvailable`, `history`. `body` excludes the title line;
`consumers` maps namespace to complete raw YAML entry text. `revision` is
lowercase SHA-256 of exact UTF-8 item bytes, not YAML/payload normalization.
Retired items have `body: null`, `bodyAvailable: false`, `retired: true` and
immutable history; no historical content is fabricated offline.
`dependenciesDone` uses the same done-only graph rule independently of current
state. Corrupt stores refuse machine access rather than claiming readiness.

`myque api create --admission TOKEN` reads
`{"title":"...","kind":"task","body":"...","consumers":{}}` from stdin.
The token is opaque. An identical request retry returns the same identity;
a changed payload with the same token refuses. MyQue alone allocates UUIDs.
The token's SHA-256 names `.tasks/admissions/<hash>.json`, retaining only request
fingerprint and UUID. Admission records are canonical idempotency data, not a
second tracker. Preserve them with the store when exchanging branches.

`myque api put UUID --expected REVISION` reads optional `body` and `consumers`.
It compares exact bytes under the write lock; only supplied namespaces change.
`myque start UUID --expected REVISION` and `myque close UUID --expected REVISION`
provide the same atomic compare-and-transition boundary for consumer eligibility.
Direct `myque close UUID` deliberately remains an operator state command: it
does not claim devloop evidence or eligibility. Consumers must use their own
eligibility check followed by guarded transition. No universal policy is imposed.

Public Haskell APIs are `Myque.Api.itemApiValue :: Store -> WorkItem -> Value`,
`apiGet`, `apiCreate`, `apiPut`, `transitionItem`, `migrateItem`, `retireItem`,
`reopenItem`; `Myque.Store` exports `TerminalRecord(..)`, `History(..)`,
`storeTerminals`, `terminalDirectory`, `terminalPath`. Pin `myque >=0.2.0.0 &&
<0.3`; projection pins exactly 0.2.0.0. Projection must never parse consumer
payloads. The serializer is pure, including after a temporary checkout is gone.

## Migration, retention and recovery

`myque migrate UUID` accepts v1 only. It first retains and verifies original
bytes in Git, then changes only `work-item/v1` to `work-item/v2` on the schema
line. `.tasks/migrations/<UUID>.json` records provenance. It neither adds a
consumer namespace nor infers requirements/evidence. Unrelated v1 work need not
migrate and migrated prose remains legacy until explicitly admitted by a consumer.

`printf '{}\n' | myque retire UUID --evidence 'closure evidence or cancellation reason'`
requires done/cancelled and nonempty evidence. Stdin is an explicit map of
retention-relevant consumer namespace entries; include only identity/projection
fields that consumer declares necessary. MyQue does not infer these fields.
The exact full original entries/body are preserved in verified Git history;
only the declared subset remains live. Reopening restores the full entries.

Terminal schema authority is `contracts/terminal-item/v1/schema.zt`.
`tools/generate-terminal.py` invokes Zutai's supported JSON API and generates
`Myque.TerminalFields` used by the JSON storage codec. Canonical records are
`.tasks/terminal/<UUID>.json`, with schema, metadata, history, evidence and
originalPath. Metadata reuses the work-item envelope codec, retaining title,
state, timestamps, graph fields and explicit consumer identities, never full
body. This is a resolution index, not an independently editable tracker.

History records store `repository` (sorted Git root commits), full `commit`,
original `path`, and `digest` (SHA-256 exact UTF-8 bytes; no payload digest).
The snapshot is created with Git object plumbing and retained under
`refs/myque/retained/<commit>` before any full item removal. It has HEAD as parent
and preserves the original path. User index and working files are not committed
implicitly. Commit the removal and terminal creation together after the operation;
never commit an intermediate journal. There is no self-referential commit digest.
Fetch/push retained refs explicitly alongside ordinary branches, e.g.
`git push origin 'refs/myque/retained/*:refs/myque/retained/*'` and
`git fetch origin 'refs/myque/retained/*:refs/myque/retained/*'`.
Git history still consumes space; the minimal terminal index grows with UUIDs.

`myque reopen UUID` retrieves and checks repository identity, full history object,
SHA-256 and UUID before restoring the active same-ID item and removing terminal
representation. Missing history and digest mismatch leave it retired with an
error. Fetch retained refs from the original repository and retry. Offline get,
projection and graph operations do not need historical content. `rm` refuses;
state or metadata writes to retired items refuse rather than losing links.

All supported readers/writers acquire an OS file lock `.tasks/write.lock`.
Before a multi-file write, `.tasks/transaction.json` records complete intent;
atomic temp-file rename publishes each member, then journal removal completes it.
After process interruption, the next locked operation replays the journal before
reading: there is one recoverable authority, not competing active/terminal items.
A malformed journal refuses all operations pending operator recovery. Concurrent
CAS writers cannot both win. Multi-item relationship writes share one journal.
Direct editor/Git writes must not run concurrently with MyQue; reconcile Git
merges and run `myque check` before resuming. This protocol covers process
interruption; filesystem power-loss durability depends on the filesystem's rename
and write guarantees. Never remove a live lock file to bypass a writer.

## Distribution and verification

Build with `cabal build all`; validate with `cabal test all`, `fourmolu --mode
check src app test`, and `hlint src app test`. Regenerate terminal bindings with
`python3 tools/generate-terminal.py` using the pinned Zutai toolchain. Package
with `cabal sdist` or `nix build`. Runtime requires Git for migration/retirement/
reopen only; active CRUD and offline graph/projection do not execute Zutai.
Release publication and retained-ref backup are explicit operator actions.
