{ compiler ? "ghc802" }:

(import ./release.nix {inherit compiler;}).podcast-chooser