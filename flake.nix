{
  description = "Fission tools";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    # nixpkgs.url = "github:NixOS/nixpkgs/release-22.05";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let

          ############
          # DEVSHELL #
          ############
          # Inspired by https://www.tweag.io/blog/2022-06-02-haskell-stack-nix-shell/
          stack-wrapped = pkgs.symlinkJoin {
            name = "stack";
            paths = [ pkgs.stack ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack \
                --add-flags "\
                  --nix \
                  --nix-pure \
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


          ################
          # BUILD OUTPUT #
          ################
          # from guide: https://github.com/input-output-hk/haskell.nix/blob/master/docs/tutorials/getting-started-flakes.md#scaffolding
          overlays = [ haskellNix.overlay
            (final: prev: {
              # This overlay adds our project to pkgs
              fission =
                final.haskell-nix.project' {
                  src = ./.;
                  compiler-nix-name = "ghc8107";
                  # This is used by `nix develop .` to open a shell for use with
                  # `cabal`, `hlint` and `haskell-language-server`
                  shell.tools = {
                    cabal = {};
                    hlint = {};
                    haskell-language-server = {};
                  };
                  # Non-Haskell shell tools go here
                  shell.buildInputs = with pkgs; [
                    nixpkgs-fmt
                  ];
                  # This adds `js-unknown-ghcjs-cabal` to the shell.
                  # shell.crossPlatforms = p: [p.ghcjs];
                };
            })
          ];
          pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
          flake = pkgs.fission.flake {
            # This adds support for `nix build .#js-unknown-ghcjs-cabal:fission:exe:fission`
            # crossPlatforms = p: [p.ghcjs];
          };
        in flake // {
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
          defaultPackage = flake.packages."fission-cli:exe:fission";
        }
      );
}
