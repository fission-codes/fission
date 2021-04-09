{ haskellNixSrc ? builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz
}:

let
  haskellNix = import haskellNixSrc { };
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;

  pkgSet = pkgs.haskell-nix.mkStackPkgSet {
    stack-pkgs = import ./nix/pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

in
  pkgSet.config.hsPkgs
