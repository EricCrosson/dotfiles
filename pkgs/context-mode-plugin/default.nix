{
  buildNpmPackage,
  fetchFromGitHub,
  nodejs,
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
    inherit nodejs;
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

    # Replace symlinks with real copies.
    #
    # start.mjs uses import.meta.url to compute __dirname, and Node.js resolves
    # symlinks before setting import.meta.url. If start.mjs is a symlink back to
    # the source, __dirname resolves to the source directory and
    # `import("./server.bundle.mjs")` loads the ORIGINAL unpatched bundle —
    # bypassing all our patches below.
    #
    # server.bundle.mjs must be a real file so substituteInPlace can modify it.
    rm $out/start.mjs $out/server.bundle.mjs
    cp ${src}/start.mjs $out/start.mjs
    cp ${src}/server.bundle.mjs $out/server.bundle.mjs

    # Pin the shebang to the exact Node.js used to compile better-sqlite3.
    # start.mjs uses #!/usr/bin/env node, which resolves whatever node is on
    # PATH at runtime. If that node differs from the one that compiled the
    # native addon, Node.js raises a NODE_MODULE_VERSION mismatch at startup.
    substituteInPlace $out/start.mjs \
      --replace-fail '#!/usr/bin/env node' '#!${nodejs}/bin/node'

    # Patch server.bundle.mjs to eliminate ~8.6s of synchronous runtime detection.
    # context-mode v1.0.25 probes for 15 runtimes via execSync("command -v <rt>")
    # then runs "<rt> --version" for each found one. On Nix, the available runtimes
    # are fixed at build time, so we pre-compute the result.
    #
    # da()  — runtime availability map (was: 15 execSync calls)
    # Et()  — version string display  (was: 7 execSync calls)
    # lp()  — bun availability check  (was: 1 execSync call)
    #
    # These patches target the minified bundle from context-mode v1.0.25.
    # The test in tests/context-mode-deps.nix verifies the patches applied.
    substituteInPlace $out/server.bundle.mjs \
      --replace-fail \
        'function da(){let t=Ne("bun");return{javascript:t?"bun":"node",typescript:t?"bun":Ne("tsx")?"tsx":Ne("ts-node")?"ts-node":null,python:Ne("python3")?"python3":Ne("python")?"python":null,shell:Vy?TO()??(Ne("sh")?"sh":Ne("powershell")?"powershell":"cmd.exe"):Ne("bash")?"bash":"sh",ruby:Ne("ruby")?"ruby":null,go:Ne("go")?"go":null,rust:Ne("rustc")?"rustc":null,php:Ne("php")?"php":null,perl:Ne("perl")?"perl":null,r:Ne("Rscript")?"Rscript":Ne("r")?"r":null,elixir:Ne("elixir")?"elixir":null}}' \
        'function da(){return{javascript:"node",typescript:null,python:"python3",shell:"bash",ruby:null,go:null,rust:null,php:null,perl:null,r:null,elixir:null}}' \
      --replace-fail \
        'function Et(t){try{return up(`''${t} --version`,{encoding:"utf-8",stdio:["pipe","pipe","pipe"],timeout:5e3}).trim().split(/\r?\n/)[0]}catch{return"unknown"}}' \
        'function Et(t){return"available"}' \
      --replace-fail \
        'function lp(){return Ne("bun")}' \
        'function lp(){return false}'
  ''
