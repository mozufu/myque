# myque

A local-first, Git-native work item tracker for tasks, issues, bugs,
milestones, epics, and follow-up work, implementing
[`docs/spec.md`](docs/spec.md) (`work-item/v1`). Packaged as a Nix flake.

Canonical state is one Markdown file per item under `.tasks/items/`, named
after the item's identity. There is no server, database, account, or central
ID allocator: identity is a UUIDv7 allocated locally, so two Git branches can
each create items with no shared registry, sequence counter, or
ID-allocation race. Human keys such as `C9.4` are presentation aliases —
relationships always persist UUIDs, so renaming a key rewrites nothing else.

```console
$ myque init
initialised tracker in /repo/.tasks

$ myque new "Userspace lifecycle supervision" --kind milestone --key C9.4 --tag runtime
created C9.4
019a10d8-8d48-7b77-a414-f95ab7af31be
.tasks/items/019a10d8-8d48-7b77-a414-f95ab7af31be.md

$ myque depend C9.4 C9.4.1     # accepts keys, writes UUIDs
$ myque next                   # open, with every dependency done
KEY     KIND  STATE         TITLE
C9.4.1  task  open (ready)  Restart policy engine

$ myque next --format id       # canonical ids, for scripts
019a10d8-8d48-7b77-a414-f95ab7af31be

$ myque check                  # 0 clean, 1 findings, 2 no tracker
checked 4 work item(s): no findings
```

A canonical item file:

```markdown
---
schema: work-item/v1
id: 019a10d8-8d48-7b77-a414-f95ab7af31be
key: C9.4
kind: milestone
state: done
created: 2026-08-20T14:21:00+08:00
closed: 2026-08-26T19:42:00+08:00
tags:
  - runtime
  - lifecycle
depends:
  - 019a018c-a43e-7cd8-903d-a45e77d78865
---

# Userspace lifecycle supervision

## Exit conditions

- A supervisor can restart a failed component.
```

## Commands

| Group | Commands |
| --- | --- |
| Storage | `init`, `check` |
| Items | `new`, `show`, `list`, `next`, `query`, `rm` |
| State | `start`, `close`, `cancel`, `reopen`, `defer`, `block` |
| Metadata | `key`, `title`, `tag`, `untag` |
| Relationships | `depend`, `undepend`, `parent`, `relate`, `unrelate`, `duplicate`, `supersede` |
| Views | `graph` (Mermaid), `render` (Markdown) |

An item is referenced by canonical UUID, by an unambiguous prefix of one, or
by human key; ids win when both could match, and an ambiguous prefix is an
error naming every candidate. `list` filters on `--state`, `--kind`, `--tag`,
`--parent`, and `--ready`; `query` takes an expression such as
`'tag = "runtime" and state != done'`. `key <item> --unset` drops an alias
without touching a single relationship. Run `myque help` for the full
listing, or `myque <command> --help` for one command's options.

A keyless item is displayed as the shortest prefix of its id that no other
id shares, widened as needed and never below eight digits, so what a table
prints always resolves back to one item. That matters because a UUIDv7
begins with its millisecond timestamp: items created in the same minute, or
backdated to one date by an import, agree through the whole first group.

`list`, `next`, and `query` also take `--format`: `id` emits one canonical
36-character id per line, and `json` emits NDJSON carrying full ids in every
relationship field. Both exist because the `KEY` column is deliberately
abbreviated — a script that needs identities should never have to parse
frontmatter or filenames.

Every value a write command accepts is validated before anything is
written, so `myque` never produces a file its own store refuses to load. An
item whose file exists but fails to decode is reported as invalid, naming
the offending field, rather than as missing.

An item is **ready** when it is `open` and every dependency is `done`.
`blocks` is normalised into the dependency relation, so either side of an
edge may declare it.

`check` validates identity (UUID form, UUIDv7, duplicates, filename/ID
mismatch), alias uniqueness, dangling references, parent and dependency
cycles, state/`closed` consistency, and schema conformance. The frontmatter
schema is closed, so an unknown field is an error rather than a warning. A
cycle finding is bounded — the start of a shortest cycle, the edge that
closes it, and the cycle's length — so a 260-item chain with one back edge
reports in a few hundred bytes rather than dumping every member.

Exit status distinguishes the failures a CI gate has to tell apart:

| Status | Meaning |
| --- | --- |
| `0` | Success. |
| `1` | `check` reported findings. They are the command's result, so they go to stdout. |
| `2` | Usage error, missing tracker, or unreadable configuration. |
| `3` | A well-formed command that could not be carried out. |

So `just tasks_check` can tell "the tracker is invalid" from "there is no
tracker here, or I am in the wrong directory".

Generated views are never authoritative: deleting them loses nothing, and
they can always be rebuilt from the Markdown files.

## Modules

| Module | Responsibility |
| --- | --- |
| `Myque.Uuid` | UUIDv7 allocation and parsing (RFC 9562 §5.7) |
| `Myque.Timestamp` | ISO 8601 timestamps; an explicit UTC offset is mandatory |
| `Myque.Frontmatter` | The closed YAML subset the schema uses; verbatim body preservation |
| `Myque.Item` | The `work-item/v1` model and its Markdown encoding |
| `Myque.Store` | Canonical file storage, configuration, and selector resolution |
| `Myque.Graph` | Parent and dependency edges, cycle detection, readiness |
| `Myque.Validate` | Repository validation findings |
| `Myque.Query` | The filter expression language |
| `Myque.Render` | Tables, detail views, Mermaid, Markdown summaries |
| `Myque.Cli` | Argument parsing and command execution |

The package depends only on GHC boot libraries: the frontmatter subset,
UUIDv7 allocation, and query language are implemented in-tree.

## Flake outputs

| Output | Description |
| --- | --- |
| `packages.default` / `packages.myque` | Library, executable, and Haddock; the cabal test-suite runs during the build |
| `packages.myque-bin` | `justStaticExecutables` — the `myque` binary without the GHC closure |
| `packages.myque-docs` | Haddock output only |
| `packages.myque-checked` | Explicitly `doCheck` + `doHaddock`, as a CI target |
| `packages.ghc` | The GHC the package is built with |
| `apps.default` / `apps.myque` | `nix run` entry point |
| `devShells.default` | `shellFor` shell with the package's dependencies, HLS, HLint, fourmolu, cabal-fmt, cabal2nix, and Hoogle |
| `checks.myque` | The package build, including the test-suite |
| `checks.myque-hlint` | HLint over `app`, `src`, `test` |
| `checks.myque-format` | `fourmolu --mode check` over all sources |
| `overlays.default` | Adds `haskellPackages.myque` to a nixpkgs instance |
| `formatter` | `nixfmt`, for `nix fmt` |

## Usage

```bash
nix build            # build the package and run its test-suite
nix run . -- list    # run the tracker
nix flake check      # package build + HLint + formatting
nix develop          # enter the dev shell
direnv allow         # or auto-enter the shell on cd
```

Inside the dev shell, the usual cabal workflow applies:

```bash
cabal build all
cabal test
cabal run myque -- check
cabal haddock
```

Formatting and linting:

```bash
fourmolu --mode inplace $(find app src test -name '*.hs')
cabal-fmt --inplace myque.cabal
hlint app src test
nix fmt
```

## Packaging notes

`myque.nix` is a checked-in `cabal2nix` output consumed with `callPackage`,
rather than a `callCabal2nix` call. `callCabal2nix` requires
import-from-derivation, which is disabled in this Nix configuration and under
restricted-eval CI. After changing dependencies in `myque.cabal`, regenerate
it and re-apply the `src` parameter:

```bash
cabal2nix ./. > myque.nix   # then restore the `src` argument, see the file header
```

The flake filters its own source tree (`lib.cleanSourceWith`), so editing
`flake.nix`, `flake.lock`, `.envrc`, or `.gitignore` does not invalidate the
package derivation.

## Consuming myque from another flake

`myque-bin` is the advertised consumer output: `justStaticExecutables`, so
its runtime closure is the binary and its libc rather than a GHC toolchain.

```nix
myque = {
  url = "github:mozufu/myque";
  inputs.nixpkgs.follows = "nixpkgs";
};
```

Then reference `myque.packages.${system}.myque-bin`.

There is no published binary cache yet, so **an uncached consumer builds
myque from source**, which means fetching a GHC toolchain first. A CI job
that runs `myque check` therefore needs either a warm Nix store or a cache;
`nixbuild/nix-quick-install-action` plus `nix-community/cache-nix-action`
keyed on `flake.lock` is the usual pairing. Budget for that before wiring
`myque check` into a job whose toolchain is otherwise just a checkout.

## License

BSD-3-Clause. See [LICENSE](LICENSE).
