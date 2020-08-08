let
  sources = import ./nix/sources.nix;
  pkgs    = import sources.nixpkgs {};
  macOSEvents = if pkgs.system == "x86_64-darwin" then pkgs.haskellPackages.hfsevents else null;
in

pkgs.mkShell {
  buildInputs = [
    pkgs.curl
    pkgs.gnumake
    pkgs.zlib

    # Data
    # pkgs.ipfs
    pkgs.haskellPackages.postgresql-libpq
    pkgs.postgresql

    macOSEvents

    # Haskell
    pkgs.ghcid
    pkgs.stack
    pkgs.stylish-haskell

    # Fun
    pkgs.figlet
    pkgs.lolcat
  ];

  shellHook = ''
    echo "Welcome to the"
    ${pkgs.figlet}/bin/figlet "Fission Server Shell" | lolcat -a -s 50
  '';
}
