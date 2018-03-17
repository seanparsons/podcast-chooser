{ mkDerivation, base, fetchgit, stdenv, vty }:
mkDerivation {
  pname = "vty-menu";
  version = "0.0.4";
  src = fetchgit {
    url = "git://github.com/timthelion/vty-menu.git";
    sha256 = "1y0d7lvqji6fakbm6xvcb4ig81qzvk1vbryah1qb3kjsxg0irjkb";
    rev = "3a7bad0b288f33fe2be7595f592c8602ddcad4b8";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base vty ];
  description = "A lib for displaying a menu and getting a selection using VTY";
  license = stdenv.lib.licenses.gpl3;
}
