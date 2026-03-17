{pkgs}: let
  contextModePlugin = pkgs.callPackage ../pkgs/context-mode-plugin {};
in
  # Beyoncé Rule: if you liked it then you shoulda put a test on it.
  # Verify all 4 external deps that start.mjs checks for are present.
  assert builtins.pathExists "${contextModePlugin}/node_modules/better-sqlite3";
  assert builtins.pathExists "${contextModePlugin}/node_modules/turndown";
  assert builtins.pathExists "${contextModePlugin}/node_modules/turndown-plugin-gfm";
  assert builtins.pathExists "${contextModePlugin}/node_modules/@mixmark-io/domino";
  # Source files needed at runtime
  assert builtins.pathExists "${contextModePlugin}/start.mjs";
  assert builtins.pathExists "${contextModePlugin}/server.bundle.mjs"; "all tests passed"
