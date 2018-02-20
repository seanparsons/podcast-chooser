{ nixpkgs }:
rec {
  compiler = nixpkgs.haskellPackages;
  resolver =
    let
      callPackage = compiler.callPackage;

      overrideFunction = self: super: rec {
        base-compat = callPackage
          (
            { mkDerivation, base, hspec, QuickCheck, stdenv, unix }:
            mkDerivation {
              pname = "base-compat";
              version = "0.9.3";
              sha256 = "7d602b0f0543fadbd598a090c738e9ce9b07a1896673dc27f1503ae3bea1a210";
              libraryHaskellDepends = [ base unix ];
              testHaskellDepends = [ base hspec QuickCheck ];
              description = "A compatibility layer for base";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        base-orphans = callPackage
          (
            { mkDerivation, base, ghc-prim, hspec, QuickCheck, stdenv }:
            mkDerivation {
              pname = "base-orphans";
              version = "0.6";
              sha256 = "c7282aa7516652e6e4a78ccdfb654a99c9da683875748ad5898a3f200be7ad0e";
              libraryHaskellDepends = [ base ghc-prim ];
              testHaskellDepends = [ base hspec QuickCheck ];
              homepage = "https://github.com/haskell-compat/base-orphans#readme";
              description = "Backwards-compatible orphan instances for base";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        base-prelude = callPackage
          (
            { mkDerivation, base, stdenv }:
            mkDerivation {
              pname = "base-prelude";
              version = "1.2.0.1";
              sha256 = "811a494f5996ff1012be15a1236cc4afb6a67fc2a9f54fdb53f4e94a8fde119e";
              libraryHaskellDepends = [ base ];
              homepage = "https://github.com/nikita-volkov/base-prelude";
              description = "The most complete prelude formed solely from the \"base\" package";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        cabal-doctest = callPackage
          (
            { mkDerivation, base, Cabal, directory, filepath, stdenv }:
            mkDerivation {
              pname = "cabal-doctest";
              version = "1.0.6";
              sha256 = "decaaa5a73eaabaf3c4f8c644bd7f6e3f428b6244e935c0cf105f75f9b24ed2d";
              libraryHaskellDepends = [ base Cabal directory filepath ];
              homepage = "https://github.com/phadej/cabal-doctest";
              description = "A Setup.hs helper for doctests running";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        call-stack = callPackage
          (
            { mkDerivation, base, nanospec, stdenv }:
            mkDerivation {
              pname = "call-stack";
              version = "0.1.0";
              sha256 = "f25f5e0992a39371079cc25c2a14b5abb872fa7d868a32753aac3a258b83b1e2";
              libraryHaskellDepends = [ base ];
              testHaskellDepends = [ base nanospec ];
              homepage = "https://github.com/sol/call-stack#readme";
              description = "Use GHC call-stacks in a backward compatible way";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        clock = callPackage
          (
            { mkDerivation, base, stdenv, tasty, tasty-quickcheck }:
            mkDerivation {
              pname = "clock";
              version = "0.7.2";
              sha256 = "886601978898d3a91412fef895e864576a7125d661e1f8abc49a2a08840e691f";
              libraryHaskellDepends = [ base ];
              testHaskellDepends = [ base tasty tasty-quickcheck ];
              homepage = "https://github.com/corsis/clock";
              description = "High-resolution clock functions: monotonic, realtime, cputime";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        colour = callPackage
          (
            { mkDerivation, base, QuickCheck, random, stdenv, test-framework
            , test-framework-quickcheck2
            }:
            mkDerivation {
              pname = "colour";
              version = "2.3.4";
              sha256 = "0f439f00b322ce3d551f28a4dd1520aa2c91d699de4cdc6d485b9b04be0dc5eb";
              enableSeparateDataOutput = true;
              libraryHaskellDepends = [ base ];
              testHaskellDepends = [
                base QuickCheck random test-framework test-framework-quickcheck2
              ];
              homepage = "http://www.haskell.org/haskellwiki/Colour";
              description = "A model for human colour/color perception";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit random; };
        dlist = callPackage
          (
            { mkDerivation, base, Cabal, deepseq, QuickCheck, stdenv }:
            mkDerivation {
              pname = "dlist";
              version = "0.8.0.4";
              sha256 = "acf1867b80cdd618b8d904e89eea33be60d3c4c3aeb80d61f29229a301cc397a";
              libraryHaskellDepends = [ base deepseq ];
              testHaskellDepends = [ base Cabal QuickCheck ];
              homepage = "https://github.com/spl/dlist";
              description = "Difference lists";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        fail = callPackage
          (
            { mkDerivation, stdenv }:
            mkDerivation {
              pname = "fail";
              version = "4.9.0.0";
              sha256 = "6d5cdb1a5c539425a9665f740e364722e1d9d6ae37fbc55f30fe3dbbbb91d4a2";
              homepage = "https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail";
              description = "Forward-compatible MonadFail class";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        integer-logarithms = callPackage
          (
            { mkDerivation, array, base, ghc-prim, integer-gmp, QuickCheck
            , smallcheck, stdenv, tasty, tasty-hunit, tasty-quickcheck
            , tasty-smallcheck
            }:
            mkDerivation {
              pname = "integer-logarithms";
              version = "1.0.2";
              sha256 = "31069ccbff489baf6c4a93cb7475640aabea9366eb0b583236f10714a682b570";
              revision = "1";
              editedCabalFile = "0sccd0d6qrcm3a7nni5lqv40g5m5knf965z4skkgbyyhb3z6qsq8";
              libraryHaskellDepends = [ array base ghc-prim integer-gmp ];
              testHaskellDepends = [
                base QuickCheck smallcheck tasty tasty-hunit tasty-quickcheck
                tasty-smallcheck
              ];
              homepage = "https://github.com/phadej/integer-logarithms";
              description = "Integer logarithms";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        monad-loops = callPackage
          (
            { mkDerivation, base, stdenv, tasty, tasty-hunit }:
            mkDerivation {
              pname = "monad-loops";
              version = "0.4.3";
              sha256 = "7eaaaf6bc43661e9e86e310ff8c56fbea16eb6bf13c31a2e28103138ac164c18";
              libraryHaskellDepends = [ base ];
              testHaskellDepends = [ base tasty tasty-hunit ];
              homepage = "https://github.com/mokus0/monad-loops";
              description = "Monadic loops";
              license = stdenv.lib.licenses.publicDomain;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        mtl = callPackage
          (
            { mkDerivation, base, stdenv, transformers }:
            mkDerivation {
              pname = "mtl";
              version = "2.2.1";
              sha256 = "cae59d79f3a16f8e9f3c9adc1010c7c6cdddc73e8a97ff4305f6439d855c8dc5";
              revision = "1";
              editedCabalFile = "0fsa965g9h23mlfjzghmmhcb9dmaq8zpm374gby6iwgdx47q0njb";
              libraryHaskellDepends = [ base transformers ];
              homepage = "http://github.com/ekmett/mtl";
              description = "Monad classes, using functional dependencies";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        parallel = callPackage
          (
            { mkDerivation, array, base, containers, deepseq, stdenv }:
            mkDerivation {
              pname = "parallel";
              version = "3.2.1.1";
              sha256 = "323bb9bc9e36fb9bfb08e68a772411302b1599bfffbc6de20fa3437ce1473c17";
              libraryHaskellDepends = [ array base containers deepseq ];
              description = "Parallel programming library";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        primitive = callPackage
          (
            { mkDerivation, base, ghc-prim, stdenv, transformers }:
            mkDerivation {
              pname = "primitive";
              version = "0.6.3.0";
              sha256 = "cddeff804e0f577f1be0179d5d145dfc170f8bfb66f663b9fba67104a45d9555";
              libraryHaskellDepends = [ base ghc-prim transformers ];
              testHaskellDepends = [ base ghc-prim ];
              homepage = "https://github.com/haskell/primitive";
              description = "Primitive memory-related operations";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        random = callPackage
          (
            { mkDerivation, base, stdenv, time }:
            mkDerivation {
              pname = "random";
              version = "1.1";
              sha256 = "b718a41057e25a3a71df693ab0fe2263d492e759679b3c2fea6ea33b171d3a5a";
              revision = "1";
              editedCabalFile = "1pv5d7bm2rgap7llp5vjsplrg048gvf0226y0v19gpvdsx7n4rvv";
              libraryHaskellDepends = [ base time ];
              testHaskellDepends = [ base ];
              description = "random number library";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        reflection = callPackage
          (
            { mkDerivation, base, stdenv, template-haskell }:
            mkDerivation {
              pname = "reflection";
              version = "2.1.3";
              sha256 = "88f81923abd7211e51de7071cd5800b30784e374c193de8cdd7b1c201f8de405";
              libraryHaskellDepends = [ base template-haskell ];
              homepage = "http://github.com/ekmett/reflection";
              description = "Reifies arbitrary terms into types that can be reflected back into terms";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        safe = callPackage
          (
            { mkDerivation, base, deepseq, QuickCheck, stdenv }:
            mkDerivation {
              pname = "safe";
              version = "0.3.16";
              sha256 = "688ae558289256aeddd8f70ca4303c36de0bb37cb70b1094a0fd4731e0235975";
              libraryHaskellDepends = [ base ];
              testHaskellDepends = [ base deepseq QuickCheck ];
              homepage = "https://github.com/ndmitchell/safe#readme";
              description = "Library of safe (exception free) functions";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        semigroups = callPackage
          (
            { mkDerivation, base, stdenv }:
            mkDerivation {
              pname = "semigroups";
              version = "0.18.4";
              sha256 = "589e3042329a6bcffb5c0e85834143586db22eb7a2aae094d492cd004f685d27";
              libraryHaskellDepends = [ base ];
              homepage = "http://github.com/ekmett/semigroups/";
              description = "Anything that associates";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        stm = callPackage
          (
            { mkDerivation, array, base, stdenv }:
            mkDerivation {
              pname = "stm";
              version = "2.4.5.0";
              sha256 = "31d7db183f13beed5c71409d12747a7f4cf3e145630553dc86336208540859a7";
              libraryHaskellDepends = [ array base ];
              homepage = "https://wiki.haskell.org/Software_transactional_memory";
              description = "Software Transactional Memory";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        text = callPackage
          (
            { mkDerivation, array, base, binary, bytestring, deepseq, directory
            , ghc-prim, HUnit, integer-gmp, QuickCheck, quickcheck-unicode
            , random, stdenv, test-framework, test-framework-hunit
            , test-framework-quickcheck2
            }:
            mkDerivation {
              pname = "text";
              version = "1.2.3.0";
              sha256 = "20e0b1627f613b32cc7f2d2e8dcc48a4a61938b24f3d14fb77cee694f0c9311a";
              libraryHaskellDepends = [
                array base binary bytestring deepseq ghc-prim integer-gmp
              ];
              testHaskellDepends = [
                array base binary bytestring deepseq directory ghc-prim HUnit
                integer-gmp QuickCheck quickcheck-unicode random test-framework
                test-framework-hunit test-framework-quickcheck2
              ];
              doCheck = false;
              homepage = "https://github.com/haskell/text";
              description = "An efficient packed Unicode text type";
              license = stdenv.lib.licenses.bsd2;
              doHaddock = false;
            }
          )
          { inherit random; };
        th-abstraction = callPackage
          (
            { mkDerivation, base, containers, ghc-prim, stdenv
            , template-haskell
            }:
            mkDerivation {
              pname = "th-abstraction";
              version = "0.2.6.0";
              sha256 = "e52e289a547d68f203d65f2e63ec2d87a3c613007d2fe873615c0969b981823c";
              libraryHaskellDepends = [
                base containers ghc-prim template-haskell
              ];
              testHaskellDepends = [ base containers template-haskell ];
              homepage = "https://github.com/glguy/th-abstraction";
              description = "Nicer interface for reified information about data types";
              license = stdenv.lib.licenses.isc;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        time-locale-compat = callPackage
          (
            { mkDerivation, base, old-locale, stdenv, time }:
            mkDerivation {
              pname = "time-locale-compat";
              version = "0.1.1.3";
              sha256 = "9144bf68b47791a2ac73f45aeadbc5910be2da9ad174909e1a10a70b4576aced";
              libraryHaskellDepends = [ base old-locale time ];
              homepage = "https://github.com/khibino/haskell-time-locale-compat";
              description = "Compatibility of TimeLocale between old-locale and time-1.5";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        transformers-compat = callPackage
          (
            { mkDerivation, base, ghc-prim, stdenv, transformers }:
            mkDerivation {
              pname = "transformers-compat";
              version = "0.5.1.4";
              sha256 = "d881ef4ec164b631591b222efe7ff555af6d5397c9d86475b309ba9402a8ca9f";
              libraryHaskellDepends = [ base ghc-prim transformers ];
              homepage = "http://github.com/ekmett/transformers-compat/";
              description = "A small compatibility shim exposing the new types from transformers 0.3 and 0.4 to older Haskell platforms.";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        unexceptionalio = callPackage
          (
            { mkDerivation, base, stdenv }:
            mkDerivation {
              pname = "unexceptionalio";
              version = "0.3.0";
              sha256 = "927e2be6bb9ced73c1c17d79c981cadef4039d9ee45d2d3d6b4c133ff93ff0b8";
              libraryHaskellDepends = [ base ];
              homepage = "https://github.com/singpolyma/unexceptionalio";
              description = "IO without any non-error, synchronous exceptions";
              license = "unknown";
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        unix-compat = callPackage
          (
            { mkDerivation, base, stdenv, unix }:
            mkDerivation {
              pname = "unix-compat";
              version = "0.5.0.1";
              sha256 = "c2f299e0439c15d93d5700911c922fd2b35543c19ba053779cd52f3b051caebd";
              libraryHaskellDepends = [ base unix ];
              homepage = "http://github.com/jystic/unix-compat";
              description = "Portable POSIX-compatibility layer";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        void = callPackage
          (
            { mkDerivation, base, stdenv }:
            mkDerivation {
              pname = "void";
              version = "0.7.2";
              sha256 = "d3fffe66a03e4b53db1e459edf75ad8402385a817cae415d857ec0b03ce0cf2b";
              libraryHaskellDepends = [ base ];
              homepage = "http://github.com/ekmett/void";
              description = "A Haskell 98 logically uninhabited data type";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        extra = callPackage
          (
            { mkDerivation, base, clock, directory, filepath, process
            , QuickCheck, stdenv, time, unix
            }:
            mkDerivation {
              pname = "extra";
              version = "1.6.3";
              sha256 = "35a898a41d7eced847c529a613b3b635a9f8172625d0615ce3926ad3a904ba19";
              libraryHaskellDepends = [
                base clock directory filepath process time unix
              ];
              testHaskellDepends = [ base directory filepath QuickCheck unix ];
              homepage = "https://github.com/ndmitchell/extra#readme";
              description = "Extra functions I use";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit clock; };
        ansi-terminal = callPackage
          (
            { mkDerivation, base, colour, stdenv }:
            mkDerivation {
              pname = "ansi-terminal";
              version = "0.7.1.1";
              sha256 = "6fc87697dfff772f7fbb4fe49e29c366b184f9ad288520831a9e0b572aa554fc";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [ base colour ];
              executableHaskellDepends = [ base ];
              homepage = "https://github.com/feuerbach/ansi-terminal";
              description = "Simple ANSI terminal support, with Windows compatibility";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit colour; };
        vector = callPackage
          (
            { mkDerivation, base, deepseq, ghc-prim, HUnit, primitive
            , QuickCheck, random, stdenv, template-haskell, test-framework
            , test-framework-hunit, test-framework-quickcheck2, transformers
            }:
            mkDerivation {
              pname = "vector";
              version = "0.12.0.1";
              sha256 = "b100ee79b9da2651276278cd3e0f08a3c152505cc52982beda507515af173d7b";
              revision = "2";
              editedCabalFile = "0vzr8kra73anchp86knkmkq2afkd1hw6hirldn9vn69frynb1n6y";
              libraryHaskellDepends = [ base deepseq ghc-prim primitive ];
              testHaskellDepends = [
                base HUnit QuickCheck random template-haskell test-framework
                test-framework-hunit test-framework-quickcheck2 transformers
              ];
              homepage = "https://github.com/haskell/vector";
              description = "Efficient Arrays";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit primitive random; };
        StateVar = callPackage
          (
            { mkDerivation, base, stdenv, stm, transformers }:
            mkDerivation {
              pname = "StateVar";
              version = "1.1.0.4";
              sha256 = "7ad68decb5c9a76f83c95ece5fa13d1b053e4fb1079bd2d3538f6b05014dffb7";
              libraryHaskellDepends = [ base stm transformers ];
              homepage = "https://github.com/haskell-opengl/StateVar";
              description = "State variables";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit stm; };
        hashable = callPackage
          (
            { mkDerivation, base, bytestring, criterion, deepseq, ghc-prim
            , HUnit, integer-gmp, QuickCheck, random, siphash, stdenv
            , test-framework, test-framework-hunit, test-framework-quickcheck2
            , text, unix
            }:
            mkDerivation {
              pname = "hashable";
              version = "1.2.6.1";
              sha256 = "94ca8789e13bc05c1582c46b709f3b0f5aeec2092be634b8606dbd9c5915bb7a";
              revision = "2";
              editedCabalFile = "0w4756sa04nk2bw3vnysb0y9d09zzg3c77aydkjfxz1hnl1dvnjn";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [
                base bytestring deepseq ghc-prim integer-gmp text
              ];
              testHaskellDepends = [
                base bytestring ghc-prim HUnit QuickCheck random test-framework
                test-framework-hunit test-framework-quickcheck2 text unix
              ];
              benchmarkHaskellDepends = [
                base bytestring criterion ghc-prim integer-gmp siphash text
              ];
              homepage = "http://github.com/tibbe/hashable";
              description = "A class for types that can be converted to a hash value";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit random text; };
        transformers-base = callPackage
          (
            { mkDerivation, base, stdenv, stm, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "transformers-base";
              version = "0.4.4";
              sha256 = "6aa3494fc70659342fbbb163035d5827ecfd8079e3c929e2372adf771fd52387";
              revision = "1";
              editedCabalFile = "196pr3a4lhgklyw6nq6rv1j9djwzmvx7xrpp58carxnb55gk06pv";
              libraryHaskellDepends = [
                base stm transformers transformers-compat
              ];
              homepage = "https://github.com/mvv/transformers-base";
              description = "Lift computations from the bottom of a transformer stack";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit stm transformers-compat; };
        tagged = callPackage
          (
            { mkDerivation, base, deepseq, stdenv, template-haskell
            , transformers, transformers-compat
            }:
            mkDerivation {
              pname = "tagged";
              version = "0.8.5";
              sha256 = "e47c51c955ed77b0fa36897f652df990aa0a8c4eb278efaddcd604be00fc8d99";
              revision = "2";
              editedCabalFile = "0r2knfcq0b4s652vlvlnfwxlc2mkc2ra9kl8bp4zdn1awmfy0ia5";
              libraryHaskellDepends = [
                base deepseq template-haskell transformers transformers-compat
              ];
              homepage = "http://github.com/ekmett/tagged";
              description = "Haskell 98 phantom types to avoid unsafely passing dummy arguments";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit transformers-compat; };
        exceptions = callPackage
          (
            { mkDerivation, base, mtl, QuickCheck, stdenv, stm
            , template-haskell, test-framework, test-framework-quickcheck2
            , transformers, transformers-compat
            }:
            mkDerivation {
              pname = "exceptions";
              version = "0.8.3";
              sha256 = "4d6ad97e8e3d5dc6ce9ae68a469dc2fd3f66e9d312bc6faa7ab162eddcef87be";
              revision = "4";
              editedCabalFile = "18iip6wffnrp1jgnf09gxg4v17ymjank50kjshxvcy9s9l9g13ln";
              libraryHaskellDepends = [
                base mtl stm template-haskell transformers transformers-compat
              ];
              testHaskellDepends = [
                base mtl QuickCheck stm template-haskell test-framework
                test-framework-quickcheck2 transformers transformers-compat
              ];
              homepage = "http://github.com/ekmett/exceptions/";
              description = "Extensible optionally-pure exceptions";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit mtl stm transformers-compat; };
        ansi-wl-pprint = callPackage
          (
            { mkDerivation, ansi-terminal, base, stdenv }:
            mkDerivation {
              pname = "ansi-wl-pprint";
              version = "0.6.8.2";
              sha256 = "a630721bd57678c3bfeb6c703f8249e434cbf85f40daceec4660fb8c6725cb3e";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [ ansi-terminal base ];
              homepage = "http://github.com/ekmett/ansi-wl-pprint";
              description = "The Wadler/Leijen Pretty Printer for colored ANSI terminal output";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit ansi-terminal; };
        vector-th-unbox = callPackage
          (
            { mkDerivation, base, data-default, stdenv, template-haskell
            , vector
            }:
            mkDerivation {
              pname = "vector-th-unbox";
              version = "0.2.1.6";
              sha256 = "be87d4a6f1005ee2d0de6adf521e05c9e83c441568a8a8b60c79efe24ae90235";
              libraryHaskellDepends = [ base template-haskell vector ];
              testHaskellDepends = [ base data-default vector ];
              description = "Deriver for Data.Vector.Unboxed using Template Haskell";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit vector; };
        vector-builder = callPackage
          (
            { mkDerivation, base, base-prelude, bug, criterion, foldl
            , QuickCheck, quickcheck-instances, rebase, rerebase, semigroups
            , stdenv, tasty, tasty-hunit, tasty-quickcheck, vector
            }:
            mkDerivation {
              pname = "vector-builder";
              version = "0.3.4.1";
              sha256 = "d1649096abdcc96894031292a63dfc0b36be4ab004a00f91f08aa5bc4c65ebb7";
              libraryHaskellDepends = [ base base-prelude semigroups vector ];
              testHaskellDepends = [
                bug criterion foldl QuickCheck quickcheck-instances rebase rerebase
                tasty tasty-hunit tasty-quickcheck
              ];
              benchmarkHaskellDepends = [ criterion foldl rerebase ];
              homepage = "https://github.com/nikita-volkov/vector-builder";
              description = "Vector builder";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit base-prelude foldl semigroups vector; };
        contravariant = callPackage
          (
            { mkDerivation, base, StateVar, stdenv, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "contravariant";
              version = "1.4.1";
              sha256 = "c93d3d619fa378f3fdf92c53bb8b04b8f47963b88aba7cfa54b57656189ad0ed";
              libraryHaskellDepends = [
                base StateVar transformers transformers-compat
              ];
              homepage = "http://github.com/ekmett/contravariant/";
              description = "Contravariant functors";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit StateVar transformers-compat; };
        uuid-types = callPackage
          (
            { mkDerivation, base, binary, bytestring, containers, criterion
            , deepseq, hashable, HUnit, QuickCheck, random, stdenv, tasty
            , tasty-hunit, tasty-quickcheck, text
            }:
            mkDerivation {
              pname = "uuid-types";
              version = "1.0.3";
              sha256 = "9276517ab24a9b06f39d6e3c33c6c2b4ace1fc2126dbc1cd9806866a6551b3fd";
              revision = "1";
              editedCabalFile = "0iwwj07gp28g357hv76k4h8pvlzamvchnw003cv3qk778pcpx201";
              libraryHaskellDepends = [
                base binary bytestring deepseq hashable random text
              ];
              testHaskellDepends = [
                base bytestring HUnit QuickCheck tasty tasty-hunit tasty-quickcheck
              ];
              benchmarkHaskellDepends = [
                base bytestring containers criterion deepseq random
              ];
              homepage = "https://github.com/aslatter/uuid";
              description = "Type definitions for Universally Unique Identifiers";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hashable random text; };
        unordered-containers = callPackage
          (
            { mkDerivation, base, bytestring, ChasingBottoms, containers
            , criterion, deepseq, deepseq-generics, hashable, hashmap, HUnit
            , mtl, QuickCheck, random, stdenv, test-framework
            , test-framework-hunit, test-framework-quickcheck2
            }:
            mkDerivation {
              pname = "unordered-containers";
              version = "0.2.9.0";
              sha256 = "6730cb5c4a3e953e2c199d6425be08fd088ff0089a3e140d63226c052e318250";
              libraryHaskellDepends = [ base deepseq hashable ];
              testHaskellDepends = [
                base ChasingBottoms containers hashable HUnit QuickCheck
                test-framework test-framework-hunit test-framework-quickcheck2
              ];
              benchmarkHaskellDepends = [
                base bytestring containers criterion deepseq deepseq-generics
                hashable hashmap mtl random
              ];
              homepage = "https://github.com/tibbe/unordered-containers";
              description = "Efficient hashing-based container types";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hashable mtl random; };
        scientific = callPackage
          (
            { mkDerivation, base, binary, bytestring, containers, criterion
            , deepseq, hashable, integer-gmp, integer-logarithms, primitive
            , QuickCheck, smallcheck, stdenv, tasty, tasty-ant-xml, tasty-hunit
            , tasty-quickcheck, tasty-smallcheck, text
            }:
            mkDerivation {
              pname = "scientific";
              version = "0.3.5.2";
              sha256 = "5ce479ff95482fb907267516bd0f8fff450bdeea546bbd1267fe035acf975657";
              revision = "4";
              editedCabalFile = "108m6b9w8l2q4r68mla9m5z47k6ahb0p68hypsmn140hgfr6a8la";
              libraryHaskellDepends = [
                base binary bytestring containers deepseq hashable integer-gmp
                integer-logarithms primitive text
              ];
              testHaskellDepends = [
                base binary bytestring QuickCheck smallcheck tasty tasty-ant-xml
                tasty-hunit tasty-quickcheck tasty-smallcheck text
              ];
              benchmarkHaskellDepends = [ base criterion ];
              homepage = "https://github.com/basvandijk/scientific";
              description = "Numbers represented using scientific notation";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hashable integer-logarithms primitive text; };
        async = callPackage
          (
            { mkDerivation, base, hashable, HUnit, stdenv, stm, test-framework
            , test-framework-hunit
            }:
            mkDerivation {
              pname = "async";
              version = "2.2.1";
              sha256 = "8f0b86022a1319d3c1c68655790da4b7f98017982e27ec3f3dbfe01029d39027";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [ base hashable stm ];
              executableHaskellDepends = [ base stm ];
              testHaskellDepends = [
                base HUnit stm test-framework test-framework-hunit
              ];
              homepage = "https://github.com/simonmar/async";
              description = "Run IO operations asynchronously and wait for their results";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hashable stm; };
        distributive = callPackage
          (
            { mkDerivation, base, base-orphans, Cabal, cabal-doctest, doctest
            , generic-deriving, hspec, stdenv, tagged, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "distributive";
              version = "0.5.3";
              sha256 = "9173805b9c941bda1f37e5aeb68ae30f57a12df9b17bd2aa86db3b7d5236a678";
              revision = "3";
              editedCabalFile = "17qqdl8p04vy314jp045100989lh84cbhqv6ghizm87xpk7ck4j3";
              setupHaskellDepends = [ base Cabal cabal-doctest ];
              libraryHaskellDepends = [
                base base-orphans tagged transformers transformers-compat
              ];
              testHaskellDepends = [ base doctest generic-deriving hspec ];
              homepage = "http://github.com/ekmett/distributive/";
              description = "Distributive functors -- Dual to Traversable";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit base-orphans cabal-doctest tagged transformers-compat; };
        temporary = callPackage
          (
            { mkDerivation, base, base-compat, directory, exceptions, filepath
            , stdenv, tasty, tasty-hunit, transformers, unix
            }:
            mkDerivation {
              pname = "temporary";
              version = "1.2.1.1";
              sha256 = "55772471e59529f4bde9555f6abb21d19a611c7d70b13befe114dc1a0ecb00f3";
              libraryHaskellDepends = [
                base directory exceptions filepath transformers unix
              ];
              testHaskellDepends = [
                base base-compat directory filepath tasty tasty-hunit unix
              ];
              homepage = "https://github.com/feuerbach/temporary";
              description = "Portable temporary file and directory support";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit base-compat exceptions; };
        errors = callPackage
          (
            { mkDerivation, base, exceptions, safe, stdenv, text, transformers
            , transformers-compat, unexceptionalio
            }:
            mkDerivation {
              pname = "errors";
              version = "2.2.2";
              sha256 = "f6322b61bf631c008410ef131f9b3f9db5a94de20e91d5339fba54461fa44e8f";
              libraryHaskellDepends = [
                base exceptions safe text transformers transformers-compat
                unexceptionalio
              ];
              description = "Simplified error-handling";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit exceptions safe text transformers-compat unexceptionalio; };
        optparse-applicative = callPackage
          (
            { mkDerivation, ansi-wl-pprint, base, bytestring, process
            , QuickCheck, stdenv, transformers, transformers-compat
            }:
            mkDerivation {
              pname = "optparse-applicative";
              version = "0.14.0.0";
              sha256 = "b55b32fdd5d101b2d6edb2746a66648fc2cd1b850d7adea185f201ac71b83c1a";
              libraryHaskellDepends = [
                ansi-wl-pprint base process transformers transformers-compat
              ];
              testHaskellDepends = [ base bytestring QuickCheck ];
              homepage = "https://github.com/pcapriotti/optparse-applicative";
              description = "Utilities and combinators for parsing command line options";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit ansi-wl-pprint transformers-compat; };
        math-functions = callPackage
          (
            { mkDerivation, base, deepseq, erf, HUnit, primitive, QuickCheck
            , stdenv, test-framework, test-framework-hunit
            , test-framework-quickcheck2, vector, vector-th-unbox
            }:
            mkDerivation {
              pname = "math-functions";
              version = "0.2.1.0";
              sha256 = "f71b5598de453546396a3f5f7f6ce877fffcc996639b7569d8628cae97da65eb";
              libraryHaskellDepends = [
                base deepseq primitive vector vector-th-unbox
              ];
              testHaskellDepends = [
                base deepseq erf HUnit primitive QuickCheck test-framework
                test-framework-hunit test-framework-quickcheck2 vector
                vector-th-unbox
              ];
              homepage = "https://github.com/bos/math-functions";
              description = "Special functions and Chebyshev polynomials";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit primitive vector vector-th-unbox; };
        attoparsec = callPackage
          (
            { mkDerivation, array, base, bytestring, case-insensitive
            , containers, criterion, deepseq, directory, filepath, ghc-prim
            , http-types, parsec, QuickCheck, quickcheck-unicode, scientific
            , stdenv, tasty, tasty-quickcheck, text, transformers
            , unordered-containers, vector
            }:
            mkDerivation {
              pname = "attoparsec";
              version = "0.13.2.2";
              sha256 = "dd93471eb969172cc4408222a3842d867adda3dd7fb39ad8a4df1b121a67d848";
              libraryHaskellDepends = [
                array base bytestring containers deepseq scientific text
                transformers
              ];
              testHaskellDepends = [
                array base bytestring deepseq QuickCheck quickcheck-unicode
                scientific tasty tasty-quickcheck text transformers vector
              ];
              benchmarkHaskellDepends = [
                array base bytestring case-insensitive containers criterion deepseq
                directory filepath ghc-prim http-types parsec scientific text
                transformers unordered-containers vector
              ];
              homepage = "https://github.com/bos/attoparsec";
              description = "Fast combinator parsing for bytestrings and text";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit scientific text unordered-containers vector; };
        comonad = callPackage
          (
            { mkDerivation, base, Cabal, cabal-doctest, containers
            , contravariant, distributive, doctest, semigroups, stdenv, tagged
            , transformers, transformers-compat
            }:
            mkDerivation {
              pname = "comonad";
              version = "5.0.3";
              sha256 = "a7f4584d634051123c547f0d10f88eaf23d99229dbd01dfdcd98cddd41e54df6";
              setupHaskellDepends = [ base Cabal cabal-doctest ];
              libraryHaskellDepends = [
                base containers contravariant distributive semigroups tagged
                transformers transformers-compat
              ];
              testHaskellDepends = [ base doctest ];
              homepage = "http://github.com/ekmett/comonad/";
              description = "Comonads";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit cabal-doctest contravariant distributive semigroups tagged transformers-compat; };
        mwc-random = callPackage
          (
            { mkDerivation, base, HUnit, math-functions, primitive, QuickCheck
            , statistics, stdenv, test-framework, test-framework-hunit
            , test-framework-quickcheck2, time, vector
            }:
            mkDerivation {
              pname = "mwc-random";
              version = "0.13.6.0";
              sha256 = "065f334fc13c057eb03ef0b6aa3665ff193609d9bfcad8068bdd260801f44716";
              libraryHaskellDepends = [
                base math-functions primitive time vector
              ];
              testHaskellDepends = [
                base HUnit QuickCheck statistics test-framework
                test-framework-hunit test-framework-quickcheck2 vector
              ];
              doCheck = false;
              homepage = "https://github.com/bos/mwc-random";
              description = "Fast, high quality pseudo random number generation";
              license = stdenv.lib.licenses.bsd3;
              doHaddock = false;
            }
          )
          { inherit math-functions primitive vector; };
        terminfo-hs = callPackage
          (
            { mkDerivation, attoparsec, base, bytestring, containers, directory
            , errors, filepath, QuickCheck, stdenv
            }:
            mkDerivation {
              pname = "terminfo-hs";
              version = "0.2.1";
              sha256 = "632fa74d6ae9fc0026022c6dd91577c8a05f95e973347b33b4d49cd9059a48ff";
              libraryHaskellDepends = [
                attoparsec base bytestring containers directory errors filepath
              ];
              testHaskellDepends = [ base directory errors filepath QuickCheck ];
              description = "A pure-Haskell (no FFI) module for accessing terminfo databases";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit attoparsec errors; };
        aeson = callPackage
          (
            { mkDerivation, attoparsec, base, base-compat, base-orphans
            , base16-bytestring, bytestring, containers, deepseq, directory
            , dlist, filepath, generic-deriving, ghc-prim, hashable
            , hashable-time, HUnit, integer-logarithms, QuickCheck
            , quickcheck-instances, scientific, stdenv, tagged
            , template-haskell, test-framework, test-framework-hunit
            , test-framework-quickcheck2, text, th-abstraction, time
            , time-locale-compat, unordered-containers, uuid-types, vector
            }:
            mkDerivation {
              pname = "aeson";
              version = "1.2.4.0";
              sha256 = "3401dba4fddb92c8a971f6645b38e2f8a1b286ef7061cd392a1a567640bbfc9b";
              libraryHaskellDepends = [
                attoparsec base base-compat bytestring containers deepseq dlist
                ghc-prim hashable scientific tagged template-haskell text
                th-abstraction time time-locale-compat unordered-containers
                uuid-types vector
              ];
              testHaskellDepends = [
                attoparsec base base-compat base-orphans base16-bytestring
                bytestring containers directory dlist filepath generic-deriving
                ghc-prim hashable hashable-time HUnit integer-logarithms QuickCheck
                quickcheck-instances scientific tagged template-haskell
                test-framework test-framework-hunit test-framework-quickcheck2 text
                time time-locale-compat unordered-containers uuid-types vector
              ];
              homepage = "https://github.com/bos/aeson";
              description = "Fast JSON parsing and encoding";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit attoparsec base-compat base-orphans dlist hashable integer-logarithms scientific tagged text th-abstraction time-locale-compat unordered-containers uuid-types vector; };
        bifunctors = callPackage
          (
            { mkDerivation, base, base-orphans, comonad, containers, hspec
            , hspec-discover, QuickCheck, semigroups, stdenv, tagged
            , template-haskell, th-abstraction, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "bifunctors";
              version = "5.5.2";
              sha256 = "332bb2ea19e77dac55282daff8046d89f69514ced5b987779d887e53b5d7cb11";
              libraryHaskellDepends = [
                base base-orphans comonad containers semigroups tagged
                template-haskell th-abstraction transformers transformers-compat
              ];
              testHaskellDepends = [
                base hspec QuickCheck template-haskell transformers
                transformers-compat
              ];
              testToolDepends = [ hspec-discover ];
              homepage = "http://github.com/ekmett/bifunctors/";
              description = "Bifunctors";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit base-orphans comonad semigroups tagged th-abstraction transformers-compat; };
        byline = callPackage
          (
            { mkDerivation, ansi-terminal, base, colour, containers, exceptions
            , haskeline, mtl, stdenv, terminfo-hs, text, transformers
            }:
            mkDerivation {
              pname = "byline";
              version = "0.3.0.0";
              sha256 = "2601d15bc201cfd1669a2095fb0efe179f3beeaf8531910fe8f5a2973a3fe20f";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [
                ansi-terminal base colour containers exceptions haskeline mtl
                terminfo-hs text transformers
              ];
              executableHaskellDepends = [ base text ];
              homepage = "http://github.com/pjones/byline";
              description = "Library for creating command-line interfaces (colors, menus, etc.)";
              license = stdenv.lib.licenses.bsd2;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit ansi-terminal colour exceptions mtl terminfo-hs text; };
        path = callPackage
          (
            { mkDerivation, aeson, base, bytestring, deepseq, exceptions
            , filepath, genvalidity, genvalidity-property, hashable, hspec, mtl
            , QuickCheck, stdenv, template-haskell, validity
            }:
            mkDerivation {
              pname = "path";
              version = "0.6.1";
              sha256 = "4b8bd85a13395b4240c639b9cf804371854d5dac69158f661068bd3089a25e59";
              libraryHaskellDepends = [
                aeson base deepseq exceptions filepath hashable template-haskell
              ];
              testHaskellDepends = [
                aeson base bytestring filepath genvalidity genvalidity-property
                hspec mtl QuickCheck validity
              ];
              description = "Support for well-typed paths";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit aeson exceptions hashable mtl; };
        semigroupoids = callPackage
          (
            { mkDerivation, base, base-orphans, bifunctors, Cabal
            , cabal-doctest, comonad, containers, contravariant, distributive
            , doctest, hashable, semigroups, stdenv, tagged, template-haskell
            , transformers, transformers-compat, unordered-containers
            }:
            mkDerivation {
              pname = "semigroupoids";
              version = "5.2.2";
              sha256 = "e4def54834cda65ac1e74e6f12a435410e19c1348e820434a30c491c8937299e";
              setupHaskellDepends = [ base Cabal cabal-doctest ];
              libraryHaskellDepends = [
                base base-orphans bifunctors comonad containers contravariant
                distributive hashable semigroups tagged template-haskell
                transformers transformers-compat unordered-containers
              ];
              testHaskellDepends = [ base doctest ];
              homepage = "http://github.com/ekmett/semigroupoids";
              description = "Semigroupoids: Category sans id";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit base-orphans bifunctors cabal-doctest comonad contravariant distributive hashable semigroups tagged transformers-compat unordered-containers; };
        profunctors = callPackage
          (
            { mkDerivation, base, base-orphans, bifunctors, comonad
            , contravariant, distributive, semigroups, stdenv, tagged
            , transformers
            }:
            mkDerivation {
              pname = "profunctors";
              version = "5.2.2";
              sha256 = "e981e6a33ac99d38a947a749179bbea3c294ecf6bfde41660fe6d8d5a2e43768";
              libraryHaskellDepends = [
                base base-orphans bifunctors comonad contravariant distributive
                semigroups tagged transformers
              ];
              homepage = "http://github.com/ekmett/profunctors/";
              description = "Profunctors";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit base-orphans bifunctors comonad contravariant distributive semigroups tagged; };
        path-io = callPackage
          (
            { mkDerivation, base, containers, directory, dlist, exceptions
            , filepath, hspec, path, stdenv, temporary, time, transformers
            , unix-compat
            }:
            mkDerivation {
              pname = "path-io";
              version = "1.3.3";
              sha256 = "2aec05914a7569f221cf73e25070fea5fad8125a9a93845e8d614a1c291e35bd";
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
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit dlist exceptions path temporary unix-compat; };
        free = callPackage
          (
            { mkDerivation, base, bifunctors, comonad, containers, distributive
            , exceptions, mtl, profunctors, semigroupoids, semigroups, stdenv
            , template-haskell, transformers, transformers-base
            , transformers-compat
            }:
            mkDerivation {
              pname = "free";
              version = "5";
              sha256 = "87916bda2ae9766c1b1b35d4fe3ed3c1bcb587e61f783776af4c5b4a2adf8ae8";
              libraryHaskellDepends = [
                base bifunctors comonad containers distributive exceptions mtl
                profunctors semigroupoids semigroups template-haskell transformers
                transformers-base transformers-compat
              ];
              homepage = "http://github.com/ekmett/free/";
              description = "Monads for free";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit bifunctors comonad distributive exceptions mtl profunctors semigroupoids semigroups transformers-base transformers-compat; };
        foldl = callPackage
          (
            { mkDerivation, base, bytestring, comonad, containers
            , contravariant, criterion, hashable, mwc-random, primitive
            , profunctors, semigroups, stdenv, text, transformers
            , unordered-containers, vector, vector-builder
            }:
            mkDerivation {
              pname = "foldl";
              version = "1.3.7";
              sha256 = "76225f77e5a63891ca9f50fdc053be1506b6508feec73003455286e9bf316984";
              libraryHaskellDepends = [
                base bytestring comonad containers contravariant hashable
                mwc-random primitive profunctors semigroups text transformers
                unordered-containers vector vector-builder
              ];
              benchmarkHaskellDepends = [ base criterion ];
              description = "Composable, streaming, and efficient left folds";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit comonad contravariant hashable mwc-random primitive profunctors semigroups text unordered-containers vector vector-builder; };
        adjunctions = callPackage
          (
            { mkDerivation, array, base, comonad, containers, contravariant
            , distributive, free, generic-deriving, hspec, hspec-discover, mtl
            , profunctors, semigroupoids, semigroups, stdenv, tagged
            , transformers, transformers-compat, void
            }:
            mkDerivation {
              pname = "adjunctions";
              version = "4.4";
              sha256 = "507c2ef55337ae61c805f8cbc1213dfd7d2b85187342675d662254b8d8a16ae9";
              libraryHaskellDepends = [
                array base comonad containers contravariant distributive free mtl
                profunctors semigroupoids semigroups tagged transformers
                transformers-compat void
              ];
              testHaskellDepends = [ base distributive generic-deriving hspec ];
              testToolDepends = [ hspec-discover ];
              homepage = "http://github.com/ekmett/adjunctions/";
              description = "Adjunctions and representable functors";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit comonad contravariant distributive free mtl profunctors semigroupoids semigroups tagged transformers-compat void; };
        kan-extensions = callPackage
          (
            { mkDerivation, adjunctions, array, base, comonad, containers
            , contravariant, distributive, fail, free, mtl, profunctors
            , semigroupoids, stdenv, tagged, transformers, transformers-compat
            }:
            mkDerivation {
              pname = "kan-extensions";
              version = "5.1";
              sha256 = "193f8e58f267663d5da8e38045b000d0983ac08b84808de42af1a44963f63205";
              libraryHaskellDepends = [
                adjunctions array base comonad containers contravariant
                distributive fail free mtl profunctors semigroupoids tagged
                transformers transformers-compat
              ];
              homepage = "http://github.com/ekmett/kan-extensions/";
              description = "Kan extensions, Kan lifts, the Yoneda lemma, and (co)density (co)monads";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit adjunctions comonad contravariant distributive fail free mtl profunctors semigroupoids tagged transformers-compat; };
        lens = callPackage
          (
            { mkDerivation, array, base, base-orphans, bifunctors, bytestring
            , Cabal, cabal-doctest, call-stack, comonad, containers
            , contravariant, criterion, deepseq, directory, distributive
            , doctest, exceptions, filepath, free, generic-deriving, ghc-prim
            , hashable, HUnit, kan-extensions, mtl, nats, parallel, profunctors
            , QuickCheck, reflection, semigroupoids, semigroups, simple-reflect
            , stdenv, tagged, template-haskell, test-framework
            , test-framework-hunit, test-framework-quickcheck2
            , test-framework-th, text, th-abstraction, transformers
            , transformers-compat, unordered-containers, vector, void
            }:
            mkDerivation {
              pname = "lens";
              version = "4.16";
              sha256 = "9f94becebbf5ef37184ad32b931648a09c7598874a26dd8f8ed5d62f8c1e9f9b";
              revision = "1";
              editedCabalFile = "0pgjpixph8idgf2wp8z25cbq6jf2bddigfs7r7nbln2a1v8yli1y";
              setupHaskellDepends = [ base Cabal cabal-doctest filepath ];
              libraryHaskellDepends = [
                array base base-orphans bifunctors bytestring call-stack comonad
                containers contravariant distributive exceptions filepath free
                ghc-prim hashable kan-extensions mtl parallel profunctors
                reflection semigroupoids semigroups tagged template-haskell text
                th-abstraction transformers transformers-compat
                unordered-containers vector void
              ];
              testHaskellDepends = [
                base bytestring containers deepseq directory doctest filepath
                generic-deriving HUnit mtl nats parallel QuickCheck semigroups
                simple-reflect test-framework test-framework-hunit
                test-framework-quickcheck2 test-framework-th text transformers
                unordered-containers vector
              ];
              benchmarkHaskellDepends = [
                base bytestring comonad containers criterion deepseq
                generic-deriving transformers unordered-containers vector
              ];
              homepage = "http://github.com/ekmett/lens/";
              description = "Lenses, Folds and Traversals";
              license = stdenv.lib.licenses.bsd2;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit base-orphans bifunctors cabal-doctest call-stack comonad contravariant distributive exceptions free hashable kan-extensions mtl parallel profunctors reflection semigroupoids semigroups tagged text th-abstraction transformers-compat unordered-containers vector void; };
        lens-aeson = callPackage
          (
            { mkDerivation, aeson, attoparsec, base, bytestring, Cabal
            , cabal-doctest, doctest, generic-deriving, lens, scientific
            , semigroups, simple-reflect, stdenv, text, unordered-containers
            , vector
            }:
            mkDerivation {
              pname = "lens-aeson";
              version = "1.0.2";
              sha256 = "4311f035caa39db3a70915a165bcbfb55ad22376085d95a9b4f57c58994702cc";
              revision = "1";
              editedCabalFile = "1xkxncl218ni4idq90g6bdd7vnwxshcpa1xk11fd3kc3i3j90pqs";
              setupHaskellDepends = [ base Cabal cabal-doctest ];
              libraryHaskellDepends = [
                aeson attoparsec base bytestring lens scientific text
                unordered-containers vector
              ];
              testHaskellDepends = [
                base doctest generic-deriving semigroups simple-reflect
              ];
              homepage = "http://github.com/lens/lens-aeson/";
              description = "Law-abiding lenses for aeson";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit aeson attoparsec cabal-doctest lens scientific semigroups text unordered-containers vector; };
      };

      newResolver = compiler.override {
        overrides = overrideFunction;
      };

    in newResolver;
}
