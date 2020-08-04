let
  sources  = import ./nix/sources.nix;
  pkgs     = import sources.nixpkgs {};
  unstable = import sources.unstable {};
  hinotify = if pkgs.stdenv.isDarwin then pkgs.hfsevents else pkgs.hinotify;
in

pkgs.mkShell {
  buildInputs = [
    pkgs.curl
    pkgs.gnumake
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
  ];
}
