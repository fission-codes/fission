{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = { name = "hs-ucan"; version = "0.0.2.0"; };
      license = "AGPL-3.0-or-later";
      copyright = "© 2021 Fission Internet Software Services for Open Networks Inc.";
      maintainer = "brooklyn@fission.codes,\ndaniel@fission.codes,\nsteven@fission.codes,\njames@fission.codes,\nbrian@fission.codes,\nphilipp@fission.codes";
      author = "Brooklyn Zelenka,\nDaniel Holmgren,\nSteven Vandevelde,\nJames Walker,\nBrian Ginsburg,\nPhilipp Krüger";
      homepage = "https://github.com/fission-suite/fission#readme";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."Glob" or (errorHandler.buildDepError "Glob"))
          (hsPkgs."HsOpenSSL" or (errorHandler.buildDepError "HsOpenSSL"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."RSA" or (errorHandler.buildDepError "RSA"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-casing" or (errorHandler.buildDepError "aeson-casing"))
          (hsPkgs."asn1-encoding" or (errorHandler.buildDepError "asn1-encoding"))
          (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base58string" or (errorHandler.buildDepError "base58string"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."cryptostore" or (errorHandler.buildDepError "cryptostore"))
          (hsPkgs."data-has" or (errorHandler.buildDepError "data-has"))
          (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
          (hsPkgs."envy" or (errorHandler.buildDepError "envy"))
          (hsPkgs."esqueleto" or (errorHandler.buildDepError "esqueleto"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."flow" or (errorHandler.buildDepError "flow"))
          (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."ipfs" or (errorHandler.buildDepError "ipfs"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."libsecp256k1" or (errorHandler.buildDepError "libsecp256k1"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mime-types" or (errorHandler.buildDepError "mime-types"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
          (hsPkgs."monad-time" or (errorHandler.buildDepError "monad-time"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."path-pieces" or (errorHandler.buildDepError "path-pieces"))
          (hsPkgs."pem" or (errorHandler.buildDepError "pem"))
          (hsPkgs."persistent" or (errorHandler.buildDepError "persistent"))
          (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
          (hsPkgs."random-bytestring" or (errorHandler.buildDepError "random-bytestring"))
          (hsPkgs."rescue" or (errorHandler.buildDepError "rescue"))
          (hsPkgs."rio" or (errorHandler.buildDepError "rio"))
          (hsPkgs."rio-orphans" or (errorHandler.buildDepError "rio-orphans"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
          (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
          (hsPkgs."servant-swagger" or (errorHandler.buildDepError "servant-swagger"))
          (hsPkgs."swagger2" or (errorHandler.buildDepError "swagger2"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
          (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
          (hsPkgs."world-peace" or (errorHandler.buildDepError "world-peace"))
          (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          ];
        libs = [ (pkgs."secp256k1" or (errorHandler.sysDepError "secp256k1")) ];
        buildable = true;
        modules = [ "Paths_hs_ucan" ];
        hsSourceDirs = [ "library" ];
        };
      tests = {
        "hs-ucan-test" = {
          depends = [
            (hsPkgs."Glob" or (errorHandler.buildDepError "Glob"))
            (hsPkgs."HsOpenSSL" or (errorHandler.buildDepError "HsOpenSSL"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."RSA" or (errorHandler.buildDepError "RSA"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-casing" or (errorHandler.buildDepError "aeson-casing"))
            (hsPkgs."asn1-encoding" or (errorHandler.buildDepError "asn1-encoding"))
            (hsPkgs."asn1-types" or (errorHandler.buildDepError "asn1-types"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base58string" or (errorHandler.buildDepError "base58string"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."cryptostore" or (errorHandler.buildDepError "cryptostore"))
            (hsPkgs."data-has" or (errorHandler.buildDepError "data-has"))
            (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
            (hsPkgs."envy" or (errorHandler.buildDepError "envy"))
            (hsPkgs."esqueleto" or (errorHandler.buildDepError "esqueleto"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."flow" or (errorHandler.buildDepError "flow"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hs-ucan" or (errorHandler.buildDepError "hs-ucan"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
            (hsPkgs."hspec-expectations" or (errorHandler.buildDepError "hspec-expectations"))
            (hsPkgs."ipfs" or (errorHandler.buildDepError "ipfs"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."libsecp256k1" or (errorHandler.buildDepError "libsecp256k1"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."mime-types" or (errorHandler.buildDepError "mime-types"))
            (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
            (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
            (hsPkgs."monad-time" or (errorHandler.buildDepError "monad-time"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."path-pieces" or (errorHandler.buildDepError "path-pieces"))
            (hsPkgs."pem" or (errorHandler.buildDepError "pem"))
            (hsPkgs."persistent" or (errorHandler.buildDepError "persistent"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."random-bytestring" or (errorHandler.buildDepError "random-bytestring"))
            (hsPkgs."rescue" or (errorHandler.buildDepError "rescue"))
            (hsPkgs."rio" or (errorHandler.buildDepError "rio"))
            (hsPkgs."rio-orphans" or (errorHandler.buildDepError "rio-orphans"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
            (hsPkgs."servant-swagger" or (errorHandler.buildDepError "servant-swagger"))
            (hsPkgs."swagger2" or (errorHandler.buildDepError "swagger2"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hspec" or (errorHandler.buildDepError "tasty-hspec"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-rerun" or (errorHandler.buildDepError "tasty-rerun"))
            (hsPkgs."tasty-smallcheck" or (errorHandler.buildDepError "tasty-smallcheck"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
            (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
            (hsPkgs."world-peace" or (errorHandler.buildDepError "world-peace"))
            (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          libs = [
            (pkgs."secp256k1" or (errorHandler.sysDepError "secp256k1"))
            ];
          buildable = true;
          modules = [ "Paths_hs_ucan" ];
          hsSourceDirs = [ "library" "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ./hs-ucan;
    }) // { cabal-generator = "hpack"; }