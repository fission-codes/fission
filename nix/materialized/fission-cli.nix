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
      identifier = { name = "fission-cli"; version = "2.20.0.0"; };
      license = "AGPL-3.0-or-later";
      copyright = "© 2022 Fission Internet Software Services for Open Networks Inc.";
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
          (hsPkgs."HsOpenSSL" or (errorHandler.buildDepError "HsOpenSSL"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."dimensions" or (errorHandler.buildDepError "dimensions"))
          (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."fission-core" or (errorHandler.buildDepError "fission-core"))
          (hsPkgs."fission-web-api" or (errorHandler.buildDepError "fission-web-api"))
          (hsPkgs."fission-web-client" or (errorHandler.buildDepError "fission-web-client"))
          (hsPkgs."fsnotify" or (errorHandler.buildDepError "fsnotify"))
          (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
          (hsPkgs."github" or (errorHandler.buildDepError "github"))
          (hsPkgs."haskeline" or (errorHandler.buildDepError "haskeline"))
          (hsPkgs."hs-ucan" or (errorHandler.buildDepError "hs-ucan"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."ipfs" or (errorHandler.buildDepError "ipfs"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
          (hsPkgs."monad-time" or (errorHandler.buildDepError "monad-time"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."open-browser" or (errorHandler.buildDepError "open-browser"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
          (hsPkgs."path-pieces" or (errorHandler.buildDepError "path-pieces"))
          (hsPkgs."rescue" or (errorHandler.buildDepError "rescue"))
          (hsPkgs."rio" or (errorHandler.buildDepError "rio"))
          (hsPkgs."rio-orphans" or (errorHandler.buildDepError "rio-orphans"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
          (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
          (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."turtle" or (errorHandler.buildDepError "turtle"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          (hsPkgs."versions" or (errorHandler.buildDepError "versions"))
          (hsPkgs."wss-client" or (errorHandler.buildDepError "wss-client"))
          (hsPkgs."wuss" or (errorHandler.buildDepError "wuss"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
          ];
        buildable = true;
        modules = [ "Paths_fission_cli" ];
        hsSourceDirs = [ "library" ];
        };
      exes = {
        "fission" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (errorHandler.buildDepError "HsOpenSSL"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."dimensions" or (errorHandler.buildDepError "dimensions"))
            (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."fission-cli" or (errorHandler.buildDepError "fission-cli"))
            (hsPkgs."fission-core" or (errorHandler.buildDepError "fission-core"))
            (hsPkgs."fission-web-api" or (errorHandler.buildDepError "fission-web-api"))
            (hsPkgs."fission-web-client" or (errorHandler.buildDepError "fission-web-client"))
            (hsPkgs."fsnotify" or (errorHandler.buildDepError "fsnotify"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."github" or (errorHandler.buildDepError "github"))
            (hsPkgs."haskeline" or (errorHandler.buildDepError "haskeline"))
            (hsPkgs."hs-ucan" or (errorHandler.buildDepError "hs-ucan"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."ipfs" or (errorHandler.buildDepError "ipfs"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
            (hsPkgs."monad-time" or (errorHandler.buildDepError "monad-time"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."open-browser" or (errorHandler.buildDepError "open-browser"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
            (hsPkgs."path-pieces" or (errorHandler.buildDepError "path-pieces"))
            (hsPkgs."rescue" or (errorHandler.buildDepError "rescue"))
            (hsPkgs."rio" or (errorHandler.buildDepError "rio"))
            (hsPkgs."rio-orphans" or (errorHandler.buildDepError "rio-orphans"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
            (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."turtle" or (errorHandler.buildDepError "turtle"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."versions" or (errorHandler.buildDepError "versions"))
            (hsPkgs."wss-client" or (errorHandler.buildDepError "wss-client"))
            (hsPkgs."wuss" or (errorHandler.buildDepError "wuss"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            ];
          buildable = true;
          modules = [ "Paths_fission_cli" ];
          hsSourceDirs = [ "cli" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "fission-cli-test" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (errorHandler.buildDepError "HsOpenSSL"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."dimensions" or (errorHandler.buildDepError "dimensions"))
            (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."fission-core" or (errorHandler.buildDepError "fission-core"))
            (hsPkgs."fission-web-api" or (errorHandler.buildDepError "fission-web-api"))
            (hsPkgs."fission-web-client" or (errorHandler.buildDepError "fission-web-client"))
            (hsPkgs."fsnotify" or (errorHandler.buildDepError "fsnotify"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."github" or (errorHandler.buildDepError "github"))
            (hsPkgs."haskeline" or (errorHandler.buildDepError "haskeline"))
            (hsPkgs."hs-ucan" or (errorHandler.buildDepError "hs-ucan"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
            (hsPkgs."hspec-expectations" or (errorHandler.buildDepError "hspec-expectations"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."ipfs" or (errorHandler.buildDepError "ipfs"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
            (hsPkgs."monad-time" or (errorHandler.buildDepError "monad-time"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."open-browser" or (errorHandler.buildDepError "open-browser"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
            (hsPkgs."path-pieces" or (errorHandler.buildDepError "path-pieces"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."rescue" or (errorHandler.buildDepError "rescue"))
            (hsPkgs."rio" or (errorHandler.buildDepError "rio"))
            (hsPkgs."rio-orphans" or (errorHandler.buildDepError "rio-orphans"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
            (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hspec" or (errorHandler.buildDepError "tasty-hspec"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-smallcheck" or (errorHandler.buildDepError "tasty-smallcheck"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."turtle" or (errorHandler.buildDepError "turtle"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."versions" or (errorHandler.buildDepError "versions"))
            (hsPkgs."wss-client" or (errorHandler.buildDepError "wss-client"))
            (hsPkgs."wuss" or (errorHandler.buildDepError "wuss"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            ];
          buildable = true;
          modules = [ "Paths_fission_cli" ];
          hsSourceDirs = [ "library" "test" ];
          mainPath = [ "Main.hs" ];
          };
        };
      benchmarks = {
        "fission-cli-benchmark" = {
          depends = [
            (hsPkgs."HsOpenSSL" or (errorHandler.buildDepError "HsOpenSSL"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."dimensions" or (errorHandler.buildDepError "dimensions"))
            (hsPkgs."dns" or (errorHandler.buildDepError "dns"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."fission-cli" or (errorHandler.buildDepError "fission-cli"))
            (hsPkgs."fission-core" or (errorHandler.buildDepError "fission-core"))
            (hsPkgs."fission-web-api" or (errorHandler.buildDepError "fission-web-api"))
            (hsPkgs."fission-web-client" or (errorHandler.buildDepError "fission-web-client"))
            (hsPkgs."fsnotify" or (errorHandler.buildDepError "fsnotify"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."github" or (errorHandler.buildDepError "github"))
            (hsPkgs."haskeline" or (errorHandler.buildDepError "haskeline"))
            (hsPkgs."hs-ucan" or (errorHandler.buildDepError "hs-ucan"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."ipfs" or (errorHandler.buildDepError "ipfs"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
            (hsPkgs."monad-time" or (errorHandler.buildDepError "monad-time"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."open-browser" or (errorHandler.buildDepError "open-browser"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
            (hsPkgs."path-pieces" or (errorHandler.buildDepError "path-pieces"))
            (hsPkgs."rescue" or (errorHandler.buildDepError "rescue"))
            (hsPkgs."rio" or (errorHandler.buildDepError "rio"))
            (hsPkgs."rio-orphans" or (errorHandler.buildDepError "rio-orphans"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
            (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
            (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
            (hsPkgs."turtle" or (errorHandler.buildDepError "turtle"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            (hsPkgs."versions" or (errorHandler.buildDepError "versions"))
            (hsPkgs."wss-client" or (errorHandler.buildDepError "wss-client"))
            (hsPkgs."wuss" or (errorHandler.buildDepError "wuss"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            ];
          buildable = true;
          modules = [ "Paths_fission_cli" ];
          hsSourceDirs = [ "benchmark" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ./fission-cli;
    }) // { cabal-generator = "hpack"; }