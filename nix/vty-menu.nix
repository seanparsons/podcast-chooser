{ mkDerivation, base, fetchgit, stdenv, vty }:
mkDerivation {
  pname = "vty-menu";
  version = "0.0.4";
  src = fetchgit {
    url = "git://github.com/seanparsons/vty-menu.git";
    sha256 = "1y48xb71sywwhvqzbr3llkfvix84n1x9d7sy6nkzv0lsm4pfz1xb";
    rev = "49760977c5e32867cf45736e2141329f68238286";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base vty ];
  description = "A lib for displaying a menu and getting a selection using VTY";
  license = stdenv.lib.licenses.gpl3;
}
