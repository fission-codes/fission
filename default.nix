let
  pkgsNix = import ./nix/pkgs.nix;
in
  # (1)
  { pkgs ? pkgsNix.native }:
    # (2)
    pkgs.haskell-nix.project {
      src = pkgs.haskell-nix.haskellLib.cleanGit {
        name = "fission-cli";
        src = ./. ;
      };
    }
