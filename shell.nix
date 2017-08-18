{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  path-io = (haskellPackages.callPackage ./path-io.nix {});

  f = { mkDerivation, aeson, base, bytestring, extra, foldl, lens
      , lens-aeson, path, sqlite-simple, stdenv, text, time
      , turtle, monad-loops
      }:
      mkDerivation {
        pname = "podcast-chooser";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          aeson base bytestring extra foldl lens lens-aeson path path-io
          sqlite-simple text time turtle monad-loops
        ];
        homepage = "https://github.com/githubuser/podcast-chooser#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
