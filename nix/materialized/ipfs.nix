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
      specVersion = "2.0";
      identifier = { name = "ipfs"; version = "1.4.2"; };
      license = "Apache-2.0";
      copyright = "© 2021 Fission Internet Software Services for Open Networks Inc.";
      maintainer = "brooklyn@fission.codes,\ndaniel@fission.codes,\nsteven@fission.codes,\njames@fission.codes";
      author = "Brooklyn Zelenka,\nDaniel Holmgren,\nSteven Vandevelde,\nJames Walker";
      homepage = "https://github.com/fission-suite/ipfs-haskell#readme";
      url = "";
      synopsis = "Access IPFS locally and remotely";
      description = "Interact with the IPFS network by shelling out to a local IPFS node or communicating via the HTTP interface of a remote IPFS node.";
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
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."envy" or (errorHandler.buildDepError "envy"))
          (hsPkgs."flow" or (errorHandler.buildDepError "flow"))
          (hsPkgs."http-media" or (errorHandler.buildDepError "http-media"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."mime-types" or (errorHandler.buildDepError "mime-types"))
          (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
          (hsPkgs."network-ip" or (errorHandler.buildDepError "network-ip"))
          (hsPkgs."regex-compat" or (errorHandler.buildDepError "regex-compat"))
          (hsPkgs."rio" or (errorHandler.buildDepError "rio"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
          (hsPkgs."servant-multipart" or (errorHandler.buildDepError "servant-multipart"))
          (hsPkgs."servant-multipart-api" or (errorHandler.buildDepError "servant-multipart-api"))
          (hsPkgs."servant-multipart-client" or (errorHandler.buildDepError "servant-multipart-client"))
          (hsPkgs."swagger2" or (errorHandler.buildDepError "swagger2"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        modules = [ "Paths_ipfs" ];
        hsSourceDirs = [ "library" ];
        };
      tests = {
        "ipfs-doctest" = {
          depends = [
            (hsPkgs."Glob" or (errorHandler.buildDepError "Glob"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."directory-tree" or (errorHandler.buildDepError "directory-tree"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."envy" or (errorHandler.buildDepError "envy"))
            (hsPkgs."flow" or (errorHandler.buildDepError "flow"))
            (hsPkgs."http-media" or (errorHandler.buildDepError "http-media"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."lens-aeson" or (errorHandler.buildDepError "lens-aeson"))
            (hsPkgs."mime-types" or (errorHandler.buildDepError "mime-types"))
            (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
            (hsPkgs."network-ip" or (errorHandler.buildDepError "network-ip"))
            (hsPkgs."regex-compat" or (errorHandler.buildDepError "regex-compat"))
            (hsPkgs."rio" or (errorHandler.buildDepError "rio"))
            (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."servant-multipart" or (errorHandler.buildDepError "servant-multipart"))
            (hsPkgs."servant-multipart-api" or (errorHandler.buildDepError "servant-multipart-api"))
            (hsPkgs."servant-multipart-client" or (errorHandler.buildDepError "servant-multipart-client"))
            (hsPkgs."swagger2" or (errorHandler.buildDepError "swagger2"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          buildable = true;
          modules = [ "Paths_ipfs" ];
          hsSourceDirs = [ "test/doctest" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ./ipfs;
    }) // { cabal-generator = "hpack"; }