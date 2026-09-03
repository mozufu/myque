# Generated with `cabal2nix ./.`, then parameterised over `src` so the flake
# can pass a source-filtered tree. Regenerate after editing myque.cabal:
#
#   cabal2nix ./. > myque.nix   # then re-apply the `src` parameter below
{
  mkDerivation,
  lib,
  src,
  base,
  hspec,
}:
mkDerivation {
  pname = "myque";
  version = "0.1.0.0";
  inherit src;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base
    hspec
  ];
  description = "An amortized O(1) purely functional FIFO queue";
  license = lib.licenses.bsd3;
  mainProgram = "myque";
}
