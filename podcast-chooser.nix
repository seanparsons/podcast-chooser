{ mkDerivation, aeson, base, bytestring, extra, foldl, lens
, lens-aeson, path, path-io, sqlite-simple, stdenv, text, time
, turtle
}:
mkDerivation {
  pname = "podcast-chooser";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring extra foldl lens lens-aeson path path-io
    sqlite-simple text time turtle
  ];
  homepage = "https://github.com/githubuser/podcast-chooser#readme";
  license = stdenv.lib.licenses.bsd3;
}
