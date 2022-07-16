{
  extras = hackage:
    {
      packages = {
        "amazonka" = (((hackage.amazonka)."1.6.1").revisions).default;
        "constraints-deriving" = (((hackage.constraints-deriving)."1.1.1.2").revisions).default;
        "cryptostore" = (((hackage.cryptostore)."0.2.1.0").revisions).default;
        "dimensions" = (((hackage.dimensions)."2.1.1.0").revisions).default;
        "hfsevents" = (((hackage.hfsevents)."0.1.6").revisions).default;
        "lzma-clib" = (((hackage.lzma-clib)."5.2.2").revisions).default;
        "raven-haskell" = (((hackage.raven-haskell)."0.1.4.0").revisions).default;
        "rescue" = (((hackage.rescue)."0.4.2.1").revisions).default;
        "servant-ekg" = (((hackage.servant-ekg)."0.3.1").revisions).default;
        "servant-multipart-client" = (((hackage.servant-multipart-client)."0.12.1").revisions).default;
        "servant-swagger-ui-redoc" = (((hackage.servant-swagger-ui-redoc)."0.3.4.1.22.3").revisions).default;
        "servant-websockets" = (((hackage.servant-websockets)."2.0.0").revisions).default;
        "unliftio-core" = (((hackage.unliftio-core)."0.1.2.0").revisions).default;
        "powerdns" = (((hackage.powerdns)."0.2.2").revisions).default;
        "github" = (((hackage.github)."0.27").revisions).default;
        fission-cli = ./fission-cli.nix;
        fission-core = ./fission-core.nix;
        fission-web-api = ./fission-web-api.nix;
        fission-web-client = ./fission-web-client.nix;
        fission-web-server = ./fission-web-server.nix;
        hs-ucan = ./hs-ucan.nix;
        ipfs = ./ipfs.nix;
        };
      };
  resolver = "lts-18.28";
  modules = [
    ({ lib, ... }:
      { packages = {}; })
    { packages = { "$everything" = { ghcOptions = [ "-haddock" ]; }; }; }
    ({ lib, ... }:
      { planned = lib.mkOverride 900 true; })
    ];
  }