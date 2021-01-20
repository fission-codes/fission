let
  sources  = import ./nix/sources.nix;
  pkgs     = import sources.nixpkgs  {};
  unstable = import sources.unstable {};
  hinotify = if pkgs.stdenv.isDarwin then pkgs.haskellPackages.hfsevents else pkgs.haskellPackages.hinotify;
in

pkgs.mkShell {
  buildInputs = [
    pkgs.curl
    pkgs.gnumake
    unstable.niv
    pkgs.zlib

    # Data
    pkgs.ipfs
    pkgs.haskellPackages.postgresql-libpq
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
}
