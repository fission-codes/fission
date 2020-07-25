let
  sources = import ./nix/sources.nix;
  pkgs    = import sources.nixpkgs {};
in

pkgs.mkShell {
  buildInputs = [
    pkgs.curl
    pkgs.gnumake
    pkgs.zlib

    # Data
    pkgs.ipfs
    pkgs.libpqxx
    pkgs.postgresql

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
