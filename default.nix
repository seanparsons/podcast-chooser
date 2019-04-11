{ compiler ? "ghc864" }:

(import ./release.nix {inherit compiler;}).podcast-chooser