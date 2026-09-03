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

$ myque check                  # non-zero exit on any finding
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

An item is referenced by canonical UUID or by human key; a well-formed UUID
always wins. `list` filters on `--state`, `--kind`, `--tag`, `--parent`, and
`--ready`; `query` takes an expression such as
`'tag = "runtime" and state != done'`. Run `myque help` for the full listing.

An item is **ready** when it is `open` and every dependency is `done`.
`blocks` is normalised into the dependency relation, so either side of an
edge may declare it.

`check` validates identity (UUID form, UUIDv7, duplicates, filename/ID
mismatch), alias uniqueness, dangling references, parent and dependency
cycles, state/`closed` consistency, and schema conformance. The frontmatter
schema is closed, so an unknown field is an error rather than a warning.
Findings exit non-zero.

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

## License

BSD-3-Clause. See [LICENSE](LICENSE).
