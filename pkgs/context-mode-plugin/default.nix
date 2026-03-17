{
  buildNpmPackage,
  fetchFromGitHub,
  python3,
  pkg-config,
  runCommand,
}: let
  # Pin to the same rev used by the fetchFromGitHub in profiles/bitgo
  src = fetchFromGitHub {
    owner = "mksglu";
    repo = "context-mode";
    rev = "040522bc782402079a387201a90352bcc3bd40e1";
    hash = "sha256-BbD0KTqtEeCqZCCaDk8NA4UwFobATabV6IekFUGTdqI=";
  };

  # Pre-build the 4 external native/JS deps that start.mjs tries to npm-install
  # at startup. better-sqlite3 has a native C++ addon (node-gyp).
  #
  # To update npmDepsHash after changing package.json:
  #   nix build .#context-mode-deps 2>&1 | grep 'got:'
  deps = buildNpmPackage {
    pname = "context-mode-deps";
    version = "1.0.0";
    src = ./.;
    npmDepsHash = "sha256-6K7nxIQQ1nDeygIWxdW6R6ZvF2/h3f12skYB9sep6XA=";
    dontNpmBuild = true;
    # better-sqlite3 needs node-gyp → python3 + pkg-config
    nativeBuildInputs = [python3 pkg-config];
    installPhase = ''
      runHook preInstall
      mkdir -p $out
      cp -r node_modules $out/node_modules
      runHook postInstall
    '';
  };
in
  # Combine the fetched source with pre-built node_modules.
  # start.mjs checks existsSync(resolve(__dirname, "node_modules", pkg))
  # so we need node_modules as a child of the plugin directory.
  runCommand "context-mode-plugin" {} ''
    cp -rs ${src} $out
    chmod -R u+w $out
    ln -s ${deps}/node_modules $out/node_modules
  ''
