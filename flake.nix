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
        overlays = [
          haskellNix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            fission =
              final.haskell-nix.project' {
                src = pkgs.haskell-nix.haskellLib.cleanSourceWith { name = "fission"; src = ./.; };
                compiler-nix-name = "ghc8107";
                stack-sha256 = "0z7k4jdpwc67vpcydf3gy3rd6v1i4qp0b0im2h3da2j2zbz59gz8";
                materialized = ./nix/materialized;
                # checkMaterialization = true;

                # NOTE: Currently handling devShells separately from haskell.nix
                #
                # shell.tools = {
                #   cabal = { };
                #   hlint = { };
                #   haskell-language-server = { };
                # };
                # Non-Haskell shell tools go here
                # shell.buildInputs = with pkgs; [
                # ];
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.fission.flake { };

        # Inspired by https://www.tweag.io/blog/2022-06-02-haskell-stack-nix-shell/
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --nix \
                --no-nix-pure \
                --nix-shell-file=nix/stack-integration.nix \
              "
          '';
        };

        # Wrapper commands for convenience
        commands = import ./nix/commands.nix;
        server-path = "~/.local/bin/fission-server";
        tasks = commands {
          inherit pkgs;
          inherit server-path;
          inherit stack-wrapped;
        };

        # The default version of HLS (with binary cache) is built with GHC 9.0.1
        # We can get this version working with our current set up, but it builds 
        # from source (and takes a long time).
        #
        # The prebuilt package is marked as broken on aarch64-darwin
        haskellPackages = pkgs.haskell.packages.ghc8107;
      in
      flake // {
        # Dev Shell
        devShells.default = pkgs.mkShell {
          name = "fission";
          buildInputs = [
            stack-wrapped
            haskellPackages.haskell-language-server
            pkgs.cachix
            pkgs.nixpkgs-fmt
            pkgs.stylish-haskell
            tasks
          ];
          NIX_PATH = "nixpkgs=" + pkgs.path;
        };

        # Built by `nix build .`
        packages.default = flake.packages."fission-cli:exe:fission";
        packages.fission-cli = flake.packages."fission-cli:exe:fission";
        packages.fission-server = flake.packages."fission-web-server:exe:fission-server";
      });

  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };
}
