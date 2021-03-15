{
  extras = hackage:
    {
      packages = {
        "alex" = (((hackage.alex)."3.2.6").revisions).default;
        "amazonka" = (((hackage.amazonka)."1.6.1").revisions).default;
        "constraints-deriving" = (((hackage.constraints-deriving)."1.1.1.1").revisions).default;
        "cryptostore" = (((hackage.cryptostore)."0.2.1.0").revisions).default;
        "dimensions" = (((hackage.dimensions)."2.1.0.0").revisions).default;
        "happy" = (((hackage.happy)."1.20.0").revisions).default;
        "hfsevents" = (((hackage.hfsevents)."0.1.6").revisions).default;
        "ipfs" = (((hackage.ipfs)."1.3.0.0").revisions).default;
        "lzma-clib" = (((hackage.lzma-clib)."5.2.2").revisions).default;
        "raven-haskell" = (((hackage.raven-haskell)."0.1.4.0").revisions).default;
        "rescue" = (((hackage.rescue)."0.2.1").revisions).default;
        "servant-auth" = (((hackage.servant-auth)."0.4.0.0").revisions).default;
        "servant-auth-server" = (((hackage.servant-auth-server)."0.4.6.0").revisions).default;
        "servant-auth-swagger" = (((hackage.servant-auth-swagger)."0.2.10.1").revisions).default;
        "servant-swagger-ui-redoc" = (((hackage.servant-swagger-ui-redoc)."0.3.3.1.22.3").revisions).default;
        "servant-websockets" = (((hackage.servant-websockets)."2.0.0").revisions).default;
        "text-time" = (((hackage.text-time)."0.3.1").revisions).default;
        "unliftio-core" = (((hackage.unliftio-core)."0.1.2.0").revisions).default;
        "bytebuild" = (((hackage.bytebuild)."0.3.7.0").revisions).default;
        "byteslice" = (((hackage.byteslice)."0.2.5.2").revisions).default;
        "bytesmith" = (((hackage.bytesmith)."0.3.7.0").revisions).default;
        "contiguous" = (((hackage.contiguous)."0.5.1").revisions).default;
        "ip" = (((hackage.ip)."1.7.3").revisions).default;
        "natural-arithmetic" = (((hackage.natural-arithmetic)."0.1.2.0").revisions).default;
        "primitive-offset" = (((hackage.primitive-offset)."0.2.0.0").revisions).default;
        "run-st" = (((hackage.run-st)."0.1.1.0").revisions).default;
        "tuples" = (((hackage.tuples)."0.1.0.0").revisions).default;
        fission-cli = ./fission-cli/fission-cli.nix;
        fission-core = ./fission-core;
        fission-web-api = ./fission-web-api/fission-web-api.nix;
        fission-web-client = ./fission-web-client/fission-web-client.nix;
        fission-web-server = ./fission-web-server/fission-web-server.nix;
        };
      };
  resolver = "lts-17.4";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }
