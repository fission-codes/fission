let
  sources  = import ./nix/sources.nix;

  nixos    = import sources.nixpkgs  {};
  darwin   = import sources.darwin   {};
  unstable = import sources.unstable {};

  pkgs     = if darwin.stdenv.isDarwin then darwin else nixos;
  ghc      = unstable.ghc;

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
      # unstable.ghcid
      unstable.ghc
      unstable.stack
      # unstable.stylish-haskell
      # unstable.haskellPackages.hie-bios
      # unstable.haskell-language-server
      # unstable.haskellPackages.implicit-hie
    ];

    fun = [
      pkgs.figlet
      pkgs.lolcat
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
    deps.fun
  ];

  shellHook = ''
    export LANG=C.UTF8
  
    echo "🌈✨ Welcome to the glorious... "
    ${pkgs.figlet}/bin/figlet "Fission Build Env" | lolcat -a -s 50
  '';
}
