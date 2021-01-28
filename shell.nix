let
  sources  = import ./nix/sources.nix;
  pkgs     = import sources.nixpkgs  {};
  unstable = import sources.unstable {};
  hinotify = if pkgs.stdenv.isDarwin then pkgs.haskellPackages.hfsevents else pkgs.haskellPackages.hinotify;
  ghc      = pkgs.ghc;
in

unstable.haskell.lib.buildStackProject {
  inherit ghc;
  name = "Fisson";
  buildInputs = [
    # Basics
    pkgs.gnumake
    unstable.niv

    # Crypto
    pkgs.openssl.dev
    pkgs.openssl.out

    # CLI
    pkgs.ncurses.dev.out

    # Data
    pkgs.ipfs
    pkgs.haskellPackages.postgresql-libpq
    pkgs.lzma.dev   
    pkgs.lzma.out
    pkgs.zlib.dev
    pkgs.zlib.out
    pkgs.postgresql

    hinotify

    # Haskell Tooling
    # unstable.ghcid
    # unstable.stack
    # unstable.stylish-haskell
    # unstable.haskellPackages.hie-bios
    # unstable.haskell-language-server
    # unstable.haskellPackages.implicit-hie

    # Fun
    pkgs.figlet
    pkgs.lolcat
  ];

   shellHook = ''
    export LANG=C.UTF8
  
    echo "Welcome to the"
    ${pkgs.figlet}/bin/figlet "Fission Shell" | lolcat -a -s 50
  '';
}
