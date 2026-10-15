{
  lib,
  rustPlatform,
}:
rustPlatform.buildRustPackage {
  pname = "rift-hotkey-bridge";
  version = "0.1.0";

  src = ./hotkey-bridge;

  cargoLock.lockFile = ./hotkey-bridge/Cargo.lock;

  # The daemon links against CoreGraphics (event taps, event posting) and
  # CoreFoundation (run loop scheduling) through raw FFI.
  env.NIX_LDFLAGS = "-framework CoreGraphics -framework CoreFoundation";

  doCheck = false;

  meta = {
    description = "Relays session-posted synthetic keystrokes to the HID event tap for rift";
    mainProgram = "rift-hotkey-bridge";
    platforms = lib.platforms.darwin;
  };
}
