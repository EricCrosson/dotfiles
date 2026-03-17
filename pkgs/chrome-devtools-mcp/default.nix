{
  fetchurl,
  nodejs,
  runCommand,
  lib,
}:
# Pre-built from npm registry — the published tarball already contains
# build/src with compiled JS and has zero runtime dependencies.
# This eliminates the ~1s npx registry check on every Claude Code startup.
let
  tarball = fetchurl {
    url = "https://registry.npmjs.org/chrome-devtools-mcp/-/chrome-devtools-mcp-0.20.1.tgz";
    hash = "sha256-zFLMZDTmzKKyygHk3B4E94h6lFpCEElooIPVsQHtTp0=";
  };
in
  runCommand "chrome-devtools-mcp-0.20.1" {
    inherit tarball;
  } ''
    mkdir -p $out/lib/node_modules/chrome-devtools-mcp $out/bin
    tar xzf $tarball --strip-components=1 -C $out/lib/node_modules/chrome-devtools-mcp

    # Patch shebangs to use Nix nodejs and make executable
    substituteInPlace $out/lib/node_modules/chrome-devtools-mcp/build/src/bin/chrome-devtools-mcp.js \
      --replace-fail '#!/usr/bin/env node' '#!${lib.getExe nodejs}'
    substituteInPlace $out/lib/node_modules/chrome-devtools-mcp/build/src/bin/chrome-devtools.js \
      --replace-fail '#!/usr/bin/env node' '#!${lib.getExe nodejs}'
    chmod +x $out/lib/node_modules/chrome-devtools-mcp/build/src/bin/chrome-devtools-mcp.js
    chmod +x $out/lib/node_modules/chrome-devtools-mcp/build/src/bin/chrome-devtools.js

    # Symlink bin entries
    ln -s $out/lib/node_modules/chrome-devtools-mcp/build/src/bin/chrome-devtools-mcp.js $out/bin/chrome-devtools-mcp
    ln -s $out/lib/node_modules/chrome-devtools-mcp/build/src/bin/chrome-devtools.js $out/bin/chrome-devtools
  ''
