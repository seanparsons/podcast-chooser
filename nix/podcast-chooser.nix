{ mkDerivation, aeson, async, base, bytestring, extra, foldl, lens
, lens-aeson, monad-loops, optparse-applicative, path, path-io
, process, stdenv, text, time, transformers, vty-menu
}:
mkDerivation {
  pname = "podcast-chooser";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bytestring extra foldl lens lens-aeson monad-loops
    optparse-applicative path path-io process text time transformers
    vty-menu
  ];
  homepage = "https://github.com/githubuser/podcast-chooser#readme";
  license = stdenv.lib.licenses.bsd3;
}
