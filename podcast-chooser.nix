{ mkDerivation, aeson, base, byline, bytestring, foldl, lens
, lens-aeson, stdenv, text, time, turtle
}:
mkDerivation {
  pname = "podcast-chooser";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base byline bytestring foldl lens lens-aeson text time turtle
  ];
  homepage = "https://github.com/githubuser/podcast-chooser#readme";
  license = stdenv.lib.licenses.bsd3;
}
