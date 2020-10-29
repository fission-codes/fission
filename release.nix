let
  # (1)
  pkgsNix         = import ./nix/pkgs.nix;
  pkgsNative      = pkgsNix.native;
  pkgsRaspberryPi = pkgsNix.crossRpi;
  pkgsArmv7l      = pkgsNix.crossArmv7l;
  pkgsMusl        = pkgsNix.crossMusl;

  # (2)
  hsApp               = import ./default.nix;
  appNative           = hsApp { pkgs = pkgsNative;      };
  appCrossRaspberryPi = hsApp { pkgs = pkgsRaspberryPi; };
  appCrossArmv7l      = hsApp { pkgs = pkgsArmv7l;      };
  appCrossMusl        = hsApp { pkgs = pkgsMusl;        };
in {
  # (3)
  native = appNative.fission-cli.components.exes.fission;
  rPi    = appCrossRaspberryPi.fission-cli.components.exes.fission;
  armv7l = appCrossArmv7l.fission-cli.components.exes.fission;
  musl   = appCrossMusl.fission-cli.components.exes.fission // {
    configureFlags = [
        "--disable-executable-dynamic"
        "--disable-shared"
        "--ghc-option=-optl=-pthread"
        "--ghc-option=-optl=-static"
        # "--ghc-option=-optl=-L${zlib.static}/lib"
      ];
  };
}
