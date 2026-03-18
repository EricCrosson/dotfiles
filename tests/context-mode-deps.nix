{
  pkgs,
  lib ? pkgs.lib,
}: let
  contextModePlugin = pkgs.callPackage ../pkgs/context-mode-plugin {};
  bundleContent = builtins.readFile "${contextModePlugin}/server.bundle.mjs";
in
  # Beyoncé Rule: if you liked it then you shoulda put a test on it.
  # Verify all 4 external deps that start.mjs checks for are present.
  assert builtins.pathExists "${contextModePlugin}/node_modules/better-sqlite3";
  assert builtins.pathExists "${contextModePlugin}/node_modules/turndown";
  assert builtins.pathExists "${contextModePlugin}/node_modules/turndown-plugin-gfm";
  assert builtins.pathExists "${contextModePlugin}/node_modules/@mixmark-io/domino";
  # Source files needed at runtime
  assert builtins.pathExists "${contextModePlugin}/start.mjs";
  assert builtins.pathExists "${contextModePlugin}/server.bundle.mjs";
  # Verify runtime detection was patched out of server.bundle.mjs.
  # If these fail, the context-mode version was likely bumped and the
  # substituteInPlace patches in pkgs/context-mode-plugin/default.nix
  # need updating to match the new minified code.
  assert !lib.hasInfix ''Ne("bun")'' bundleContent;
  assert !lib.hasInfix "up(`\${t} --version`" bundleContent; "all tests passed"
