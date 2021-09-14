let
  sources  = import ./nix/sources.nix;
  commands = import ./nix/commands.nix;

  nixos    = import sources.nixos    {};
  darwin   = import sources.darwin   {};
  unstable = import sources.unstable {};

  pkgs  = if darwin.stdenv.isDarwin then darwin else nixos;

in
  pkgs.mkShell {
    nativeBuildInputs = [
      pkgs.wrk2
      pkgs.nodejs
      pkgs.yarn
    ];

    shellHook = ''
      export LANG=C.UTF8

      echo "Setting up openapi-diff"
      yarn install openapi-diff
    '';
  }
