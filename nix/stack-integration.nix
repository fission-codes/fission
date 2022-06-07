{ ghc }:
with (import <nixpkgs> { });

haskell.lib.buildStackProject {
  inherit ghc;
  name = "fission";
  buildInputs = [
    lzma
    openssl
    postgresql
    zlib

    # TODO: this should only be on darwin
    darwin.apple_sdk.frameworks.CoreServices
    darwin.apple_sdk.frameworks.Foundation
    darwin.apple_sdk.frameworks.Cocoa
  ];
}
