# myque

An amortized O(1) purely functional FIFO queue for Haskell, packaged as a Nix
flake.

`Myque.Queue` is a banker's queue: two lists, `front` in dequeue order and
`back` in reverse enqueue order. `push` conses onto `back`, `pop` unconses
`front`, and `front` is refilled with `reverse back` when it runs dry. The
representation maintains the invariant

```
null front  ==>  null back
```

so `null`, `peek`, and `size` are O(1), and every element is reversed at most
once, making `push` and `pop` amortized O(1).

## Flake outputs

| Output | Description |
| --- | --- |
| `packages.default` / `packages.myque` | Library, executable, and Haddock; the cabal test-suite runs during the build |
| `packages.myque-bin` | `justStaticExecutables` — the `myque` binary without the GHC closure (55 MB vs 3.5 GB) |
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
nix build            # build the package and run its tests
nix run . -- a b c   # enqueue arguments, then drain the queue
nix flake check      # package build + HLint + formatting
nix develop          # enter the dev shell
direnv allow         # or auto-enter the shell on cd
```

Inside the dev shell, the usual cabal workflow applies:

```bash
cabal build all
cabal test
cabal run myque -- alpha beta
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

## API

```haskell
empty      :: Queue a
singleton  :: a -> Queue a
fromList   :: [a] -> Queue a
push       :: a -> Queue a -> Queue a
pop        :: Queue a -> Maybe (a, Queue a)
peek       :: Queue a -> Maybe a
null       :: Queue a -> Bool
size       :: Queue a -> Int
toList     :: Queue a -> [a]
```

`Queue` instantiates `Foldable`, `Functor`, `Traversable`, `Semigroup`,
`Monoid`, `Eq`, `Ord`, and `Show`. `Eq`/`Ord` compare the logical element
sequence, so the internal front/back split is not observable:

```haskell
>>> foldl (flip push) empty [1, 2, 3] == fromList [1, 2, 3]
True
```

## License

BSD-3-Clause. See [LICENSE](LICENSE).
