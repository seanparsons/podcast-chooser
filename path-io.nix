{ mkDerivation, base, containers, directory, dlist, exceptions
, filepath, hspec, path, stdenv, temporary, time, transformers
, unix-compat
}:
mkDerivation {
  pname = "path-io";
  version = "1.3.3";
  sha256 = "1g9m3qliqjk1img894wsb89diym5zrq51qkkrwhz4sbm9a8hbv1a";
  libraryHaskellDepends = [
    base containers directory dlist exceptions filepath path temporary
    time transformers unix-compat
  ];
  testHaskellDepends = [
    base directory exceptions hspec path transformers unix-compat
  ];
  homepage = "https://github.com/mrkkrp/path-io";
  description = "Interface to ‘directory’ package for users of ‘path’";
  license = stdenv.lib.licenses.bsd3;
}
