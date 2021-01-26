let
  sources  = import ./nix/sources.nix;
  pkgs     = import sources.nixpkgs  {};
  unstable = import sources.unstable {};
  hinotify = if pkgs.stdenv.isDarwin then pkgs.haskellPackages.hfsevents else pkgs.haskellPackages.hinotify;
in

pkgs.mkShell {
  nativeBuildInputs = [pkgs.pkg-config];

  buildInputs = [
    pkgs.figlet
    pkgs.lolcat

    pkgs.curl
    pkgs.gnumake
    unstable.niv

    pkgs.lzma.dev   
    pkgs.lzma.out

    # pkgs.zlib.dev
    # pkgs.zlib.out

    # Data
    pkgs.ipfs
    pkgs.haskellPackages.postgresql-libpq
    pkgs.openssl.dev
    pkgs.openssl.out
    pkgs.postgresql

    hinotify

    # Haskell
    unstable.ghcid
    unstable.stack
    unstable.stylish-haskell
    unstable.haskellPackages.hie-bios
    unstable.haskell-language-server
    unstable.haskellPackages.implicit-hie
  ];

  shellHook = ''
    export LANG=C.UTF8

    echo "Welcome to the"
    ${pkgs.figlet}/bin/figlet "Fission Project Shell" | lolcat -a -s 50
  '';
}
