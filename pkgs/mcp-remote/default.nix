{
  buildNpmPackage,
  importNpmLock,
  nodejs,
}:
# Pin mcp-remote so the Linear MCP server runs a hermetic, integrity-checked
# binary instead of `npx mcp-remote` (which needs a Node toolchain on PATH and
# does a registry check on every startup).
#
# importNpmLock derives a fixed-output derivation per dependency from the
# `integrity` fields in package-lock.json — there is NO npmDepsHash to keep in
# sync, so Renovate can bump package.json + package-lock.json and the build
# stays green with no manual step.
buildNpmPackage {
  pname = "mcp-remote";
  version = "0.1.38";
  src = ./.;

  npmDeps = importNpmLock {npmRoot = ./.;};
  inherit (importNpmLock) npmConfigHook;
  inherit nodejs;

  # mcp-remote ships a prebuilt bundled dist/ — nothing to compile.
  dontNpmBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib $out/bin
    cp -r node_modules $out/lib/node_modules

    # buildNpmPackage's patchShebangs has already pinned dist/proxy.js to the
    # Nix nodejs, so the server never resolves an ambient `node` from PATH.
    chmod +x $out/lib/node_modules/mcp-remote/dist/proxy.js
    ln -s $out/lib/node_modules/mcp-remote/dist/proxy.js $out/bin/mcp-remote

    runHook postInstall
  '';
}
