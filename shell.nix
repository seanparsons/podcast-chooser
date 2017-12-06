{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  path-io = (pkgs.haskellPackages.callPackage ./path-io.nix {});

  f = { mkDerivation, aeson, async, base, bytestring, extra, foldl
      , lens, lens-aeson, monad-loops, optparse-applicative, path
      , process, stdenv, text, time, transformers
      }:
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
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
