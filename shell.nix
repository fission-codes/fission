let
  sources  = import ./nix/sources.nix;
  commands = import ./nix/commands.nix;

  nixos    = import sources.nixpkgs  {};
  darwin   = import sources.darwin   {};
  unstable = import sources.unstable {};

  pkgs  = if darwin.stdenv.isDarwin then darwin else nixos;
  tasks = commands {
    inherit pkgs;
    inherit unstable;
    inherit server-path;
    inherit server-port;
  };

  server-path = "~/.local/bin/fission-server";
  server-port = 10235;

  deps = {
    bench = [pkgs.wrk2];

    common = [ 
      pkgs.gnumake
      unstable.niv
    ];

    crypto = [ 
      pkgs.openssl.dev
      pkgs.openssl.out
    ];

    cli = [pkgs.ncurses.dev.out];

    data = [
      pkgs.ipfs
      pkgs.lzma.dev   
      pkgs.lzma.out
      pkgs.zlib.dev
      pkgs.zlib.out
      pkgs.postgresql
    ];

    haskell = [
      unstable.haskellPackages.implicit-hie
      unstable.haskell-language-server
      unstable.stack
      unstable.stylish-haskell
    ];

    macos =
      if pkgs.stdenv.isDarwin then
        [ unstable.darwin.apple_sdk.frameworks.CoreServices
          unstable.darwin.apple_sdk.frameworks.Foundation
          unstable.darwin.apple_sdk.frameworks.Cocoa
        ]
      else 
        [];
  };
in

unstable.haskell.lib.buildStackProject {
  name = "Fission";
  nativeBuildInputs = builtins.concatLists [
    deps.common 
    deps.crypto
    deps.cli
    deps.data
    deps.macos
    deps.haskell
    tasks
  ];

  shellHook = ''
    export LANG=C.UTF8
  '';
}
