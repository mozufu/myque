# Generated with `cabal2nix ./.`, then parameterised over `src` so the flake
# can pass a source-filtered tree. Regenerate after editing myque.cabal:
#
#   cabal2nix ./. > myque.nix   # then re-apply the `src` parameter below
{
  mkDerivation,
  lib,
  src,
  base,
  aeson,
  yaml,
  crypton,
  filelock,
  process,
  bytestring,
  containers,
  directory,
  filepath,
  hspec,
  text,
  time,
}:
mkDerivation {
  pname = "myque";
  version = "0.2.0.0";
  inherit src;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    aeson
    yaml
    crypton
    filelock
    process
    bytestring
    containers
    directory
    filepath
    text
    time
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base
    aeson
    containers
    bytestring
    process
    directory
    filepath
    hspec
    text
  ];
  description = "A local-first, Git-native work item tracker";
  license = lib.licenses.bsd3;
  mainProgram = "myque";
}
