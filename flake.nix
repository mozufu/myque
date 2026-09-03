{
  description = "myque - an amortized O(1) purely functional FIFO queue";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];

      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

      # Sources that do not affect the build are filtered out so that editing
      # e.g. flake.nix or .envrc does not invalidate the package derivation.
      src =
        let
          inherit (nixpkgs) lib;
        in
        lib.cleanSourceWith {
          name = "myque-src";
          src = lib.cleanSource ./.;
          filter =
            path: type:
            let
              rel = lib.removePrefix (toString ./. + "/") (toString path);
              base = baseNameOf path;
            in
            !(
              (type == "directory" && (base == ".direnv" || base == "dist-newstyle"))
              || lib.hasPrefix "result" base
              || base == "flake.nix"
              || base == "flake.lock"
              || base == ".envrc"
              || base == ".gitignore"
              || lib.hasSuffix ".nix" rel
            );
        };

      # `haskellPackages` tracks the nixpkgs default GHC, which is the variant
      # with the best binary-cache coverage for haskell-language-server.
      hsPkgsFor = pkgs: pkgs.haskellPackages;

      # ./myque.nix is a committed cabal2nix output rather than a
      # `callCabal2nix` call: the latter needs import-from-derivation, which is
      # disabled here (and in restricted-eval CI). Regenerate it after
      # changing dependencies in myque.cabal.
      myqueFor = pkgs: (hsPkgsFor pkgs).callPackage ./myque.nix { inherit src; };
    in
    {
      packages = forAllSystems (
        pkgs:
        let
          hsPkgs = hsPkgsFor pkgs;
          myque = myqueFor pkgs;
        in
        {
          default = myque;
          myque = myque;

          # Just the `myque` executable, without the library, docs, or the
          # GHC closure in the runtime dependencies.
          myque-bin = pkgs.haskell.lib.compose.justStaticExecutables myque;

          myque-docs = myque.doc;

          # Haddock + coverage-style artifacts, useful as a CI target.
          myque-checked = pkgs.haskell.lib.compose.doCheck (pkgs.haskell.lib.compose.doHaddock myque);

          inherit (hsPkgs) ghc;
        }
      );

      apps = forAllSystems (pkgs: rec {
        default = myque;
        myque = {
          type = "app";
          program = "${pkgs.haskell.lib.compose.justStaticExecutables (myqueFor pkgs)}/bin/myque";
        };
      });

      devShells = forAllSystems (
        pkgs:
        let
          hsPkgs = hsPkgsFor pkgs;
        in
        {
          # `shellFor` pulls in the package's own dependencies from the same
          # package set that builds `packages.default`, so `cabal build` in the
          # shell resolves against exactly the pinned versions.
          default = hsPkgs.shellFor {
            name = "myque-shell";
            packages = _: [ (myqueFor pkgs) ];

            withHoogle = true;

            nativeBuildInputs = [
              pkgs.cabal-install
              pkgs.haskell-language-server
              hsPkgs.hlint
              hsPkgs.fourmolu
              hsPkgs.cabal-fmt
              hsPkgs.cabal2nix
              pkgs.pkg-config
              pkgs.zlib
            ];

            shellHook = ''
              echo "myque dev shell: ghc $(ghc --numeric-version) / cabal $(cabal --numeric-version)"
            '';
          };
        }
      );

      checks = forAllSystems (
        pkgs:
        let
          myque = myqueFor pkgs;
        in
        {
          # The cabal test-suite runs as part of the package build.
          myque = myque;

          myque-hlint =
            pkgs.runCommand "myque-hlint"
              {
                nativeBuildInputs = [ (hsPkgsFor pkgs).hlint ];
              }
              ''
                cd ${src}
                hlint app src test
                touch "$out"
              '';

          myque-format =
            pkgs.runCommand "myque-format"
              {
                nativeBuildInputs = [ (hsPkgsFor pkgs).fourmolu ];
              }
              ''
                cd ${src}
                fourmolu --mode check $(find app src test -name '*.hs')
                touch "$out"
              '';
        }
      );

      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
            hfinal: _hprev: {
              myque = hfinal.callPackage ./myque.nix { inherit src; };
            }
          );
        });
      };

      formatter = forAllSystems (pkgs: pkgs.nixfmt);
    };
}
