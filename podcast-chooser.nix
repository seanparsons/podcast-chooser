{ mkDerivation, aeson, async, base, bytestring, extra, foldl, lens
, lens-aeson, monad-loops, optparse-applicative, path, path-io
, process, stdenv, text, time, transformers
, nixpkgs ? import <nixpkgs> {}
}:
let
  inherit (nixpkgs) pkgs;
  path-io = (pkgs.haskellPackages.callPackage ./path-io.nix {});
in
  mkDerivation {
    pname = "podcast-chooser";
    version = "0.1.0.0";
    src = ./.;
    isLibrary = false;
    isExecutable = true;
    executableHaskellDepends = [
      aeson async base bytestring extra foldl lens lens-aeson monad-loops
      optparse-applicative path path-io process text time
      transformers
    ];
    homepage = "https://github.com/githubuser/podcast-chooser#readme";
    license = stdenv.lib.licenses.bsd3;
  }
