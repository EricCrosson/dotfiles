{pkgs}: let
  contextModePlugin = pkgs.callPackage ../pkgs/context-mode-plugin {};
in
  # Beyoncé Rule: if you liked it then you shoulda put a test on it.
  pkgs.runCommand "context-mode-deps-test" {} ''
    # Verify all 4 external deps that start.mjs checks for are present.
    test -d ${contextModePlugin}/node_modules/better-sqlite3
    test -d ${contextModePlugin}/node_modules/turndown
    test -d ${contextModePlugin}/node_modules/turndown-plugin-gfm
    test -d ${contextModePlugin}/node_modules/@mixmark-io/domino
    # Source files needed at runtime
    test -f ${contextModePlugin}/start.mjs
    test -f ${contextModePlugin}/server.bundle.mjs

    # Verify start.mjs shebang is pinned to the Nix store node, not #!/usr/bin/env node.
    # A PATH-resolved node could differ from the node used to compile better-sqlite3,
    # causing a NODE_MODULE_VERSION mismatch at startup.
    head -1 ${contextModePlugin}/start.mjs | grep -q '^#!/nix/store/'
    ! grep -q '#!/usr/bin/env node' ${contextModePlugin}/start.mjs

    # Verify plugin.json MCP server command is pinned to Nix store node.
    # Claude Code uses this field (not the shebang) to spawn the plugin process.
    grep -q '/nix/store/' ${contextModePlugin}/.claude-plugin/plugin.json
    ! grep -q '"command": "node"' ${contextModePlugin}/.claude-plugin/plugin.json

    # Verify start.mjs no longer imports ensure-deps or calls ensureNativeCompat.
    # Both try to npm-install/rebuild native deps, which is unnecessary (pre-built by Nix)
    # and would silently fail against the read-only Nix store at startup.
    ! grep -q 'import { ensureDeps }' ${contextModePlugin}/start.mjs
    ! grep -q 'ensureNativeCompat(__dirname)' ${contextModePlugin}/start.mjs

    # Verify runtime detection was patched out of server.bundle.mjs.
    # If these fail, the context-mode version was likely bumped and the
    # substituteInPlace patches in pkgs/context-mode-plugin/default.nix
    # need updating to match the new minified code.
    # xa() patched (was: runtime availability map with execSync probes)
    ! grep -q 'function xa(){let e=ux()' ${contextModePlugin}/server.bundle.mjs
    # ux() patched (was: bun availability check)
    ! grep -q 'function ux(){if(Ae' ${contextModePlugin}/server.bundle.mjs
    # Et() patched (was: execSync "''${t} --version")
    ! grep -q 'yp(`''${t} --version`' ${contextModePlugin}/server.bundle.mjs

    touch $out
  ''
