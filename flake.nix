{
  description = "Fission tools";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, flake-compat }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ] (system:
      let
        # Wrapper commands for convenience
        commands = import ./nix/commands.nix;
        server-path = "~/.local/bin/fission-server";
        tasks = commands {
          inherit pkgs;
          inherit server-path;
          # inherit stack-wrapped;
        };

        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            fission =
              final.haskell-nix.project'
                {
                  src = pkgs.haskell-nix.haskellLib.cleanSourceWith { name = "fission"; src = ./.; };
                  compiler-nix-name = "ghc8107";
                  stack-sha256 = "0z7k4jdpwc67vpcydf3gy3rd6v1i4qp0b0im2h3da2j2zbz59gz8";
                  materialized = ./nix/materialized;
                  # checkMaterialization = true;

                  # NOTE: Currently handling devShells separately from haskell.nix
                  #
                  shell.tools = {
                    cabal = "3.8.1.0";
                    # hlint = "3.4.1";
                    stack = "2.9.1";
                    # Currently running into https://github.com/input-output-hk/haskell.nix/issues/1830
                    # haskell-language-server = "1.9.0.0";
                  };

                  # From: https://github.com/input-output-hk/haskell.nix/issues/1759#issuecomment-1286299368
                  shell.additional = ps: with ps; [ Cabal ];

                  # Non-Haskell shell tools go here
                  shell.buildInputs = with pkgs; [
                    cachix
                    nixpkgs-fmt
                    tasks
                  ];

                };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.fission.flake { };
      in
      flake // {
        # Built by `nix build .`
        packages.default = flake.packages."fission-cli:exe:fission";
        packages.fission-cli = flake.packages."fission-cli:exe:fission";
        packages.fission-server = flake.packages."fission-web-server:exe:fission-server";
      });

  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [ "https://cache.iog.io" "https://fission-codes.cachix.org" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "fission-codes.cachix.org-1:z9T3OvxxngfRrx/TelcOzdnceJaCaqKQ0fby3GV1VFw=" ];
    allow-import-from-derivation = "true";
  };
}
