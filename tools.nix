{ rosetta ? false }:
  let
    sources  = import ./nix/sources.nix;
    commands = import ./nix/commands.nix;

    overrides = if rosetta then { system = "x86_64-darwin"; } else {};

    nixos    = import sources.nixos    overrides;
    darwin   = import sources.darwin   overrides;
    unstable = import sources.unstable overrides;

    pkgs     = if darwin.stdenv.isDarwin then darwin else nixos;
    loadtest = if pkgs.stdenv.isLinux then [pkgs.wrk2] else [];

  in
    pkgs.mkShell {
      nativeBuildInputs = [
        pkgs.nodejs
        pkgs.yarn
      ] ++ loadtest;

      shellHook = ''
        export LANG=C.UTF8

        echo " Setting up openapi-diff"
        yarn add openapi-diff
        alias openapi-diff="./node_modules/openapi-diff/bin/openapi-diff"
      '';
    }
