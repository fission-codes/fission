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

  server-path = "~/.local/bin/server";
  server-port = 10235;

  ghc = unstable.ghc;

  deps = {
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
      pkgs.haskellPackages.postgresql-libpq
      pkgs.lzma.dev   
      pkgs.lzma.out
      pkgs.zlib.dev
      pkgs.zlib.out
      pkgs.postgresql
    ];

    haskell = [
      unstable.ghcid
      unstable.ghc
      unstable.stack
      unstable.stylish-haskell
      unstable.haskellPackages.hie-bios
      unstable.haskell-language-server
      unstable.haskellPackages.implicit-hie
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
  inherit ghc;
  name = "Fisson";
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
    touch ${server-path}
  '';
}
