{ mkDerivation, aeson, async, base, byline, bytestring, extra
, foldl, lens, lens-aeson, monad-loops, optparse-applicative, path
, path-io, process, stdenv, text, time, transformers
}:
mkDerivation {
  pname = "podcast-chooser";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base byline bytestring extra foldl lens lens-aeson
    monad-loops optparse-applicative path path-io process text time
    transformers
  ];
  homepage = "https://github.com/githubuser/podcast-chooser#readme";
  license = stdenv.lib.licenses.bsd3;
}
