let
  sources  = import ./nix/sources.nix;
  pkgs     = import sources.nixpkgs  {};
  unstable = import sources.unstable {};
  hinotify = if pkgs.stdenv.isDarwin then pkgs.haskellPackages.hfsevents else pkgs.haskellPackages.hinotify;
  ghc      = unstable.ghc;
in

# unstable.mkShell {
unstable.haskell.lib.buildStackProject {
  inherit ghc;
  name = "Fisson";
  nativeBuildInputs = [
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
    unstable.ghc
    unstable.stack
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
