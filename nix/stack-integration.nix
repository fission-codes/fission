{ ghc }:
with (import <nixpkgs> { });
let
  macos =
    if stdenv.isDarwin then
      [
        darwin.apple_sdk.frameworks.CoreServices
        darwin.apple_sdk.frameworks.Foundation
        darwin.apple_sdk.frameworks.Cocoa
      ]
    else
      [ ];
in
haskell.lib.buildStackProject {
  inherit ghc;
  name = "fission";
  buildInputs = [
    lzma
    openssl
    postgresql
    secp256k1
    zlib

    # https://nixos.wiki/wiki/FAQ/I_installed_a_library_but_my_compiler_is_not_finding_it._Why%3F
    # Needed for binding secp256k1
    pkg-config
  ] ++ macos;
}
