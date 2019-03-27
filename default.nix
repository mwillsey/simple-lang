with import <nixpkgs> {
  overlays = [
    # set up the rust overlay for up-to-date rust versions
    (import (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz))
  ];
};
let
  # using rust overlay
  rustChannel = rustChannelOf { channel = "1.32.0"; };
  rust = rustChannel.rust.override {
    extensions = ["rust-src"];
  };
in
stdenv.mkDerivation {
  name = "lang";
  buildInputs = [
    rust
    rustracer
  ];
  RUST_SRC_PATH = "${rust}/lib/rustlib/src/rust/src";
}
