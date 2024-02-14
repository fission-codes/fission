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
      identifier = { name = "fission-web-api"; version = "2.0.1.0"; };
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
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."fission-core" or (errorHandler.buildDepError "fission-core"))
          (hsPkgs."flow" or (errorHandler.buildDepError "flow"))
          (hsPkgs."hs-ucan" or (errorHandler.buildDepError "hs-ucan"))
          (hsPkgs."http-media" or (errorHandler.buildDepError "http-media"))
          (hsPkgs."ipfs" or (errorHandler.buildDepError "ipfs"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."rio" or (errorHandler.buildDepError "rio"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-auth" or (errorHandler.buildDepError "servant-auth"))
          (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
          (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
          (hsPkgs."servant-ekg" or (errorHandler.buildDepError "servant-ekg"))
          (hsPkgs."servant-swagger" or (errorHandler.buildDepError "servant-swagger"))
          (hsPkgs."servant-swagger-ui-core" or (errorHandler.buildDepError "servant-swagger-ui-core"))
          (hsPkgs."servant-websockets" or (errorHandler.buildDepError "servant-websockets"))
          (hsPkgs."swagger2" or (errorHandler.buildDepError "swagger2"))
          (hsPkgs."uuid" or (errorHandler.buildDepError "uuid"))
          (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
          ];
        buildable = true;
        modules = [ "Paths_fission_web_api" ];
        hsSourceDirs = [ "library" ];
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ./fission-web-api;
    }) // { cabal-generator = "hpack"; }