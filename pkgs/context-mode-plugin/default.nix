{
  buildNpmPackage,
  fetchFromGitHub,
  nodejs,
  python3,
  pkg-config,
  runCommand,
}: let
  src = fetchFromGitHub {
    owner = "mksglu";
    repo = "context-mode";
    rev = "08bd59e968f29d2dda70e26e95f850a34295e54a"; # v1.0.75
    hash = "sha256-qZUZ/K5wq33gReXjPM7jVLA8KQQZ2iU4evboq6YbPLA=";
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
    rm $out/start.mjs $out/server.bundle.mjs $out/.claude-plugin/plugin.json
    cp ${src}/start.mjs $out/start.mjs
    cp ${src}/server.bundle.mjs $out/server.bundle.mjs
    cp ${src}/.claude-plugin/plugin.json $out/.claude-plugin/plugin.json

    # Pin the MCP server command to the exact Node.js that compiled better-sqlite3.
    # Claude Code spawns the plugin via "command" in plugin.json, which defaults to
    # bare "node" (resolved from PATH). If PATH node differs from the build-time
    # node, Node.js raises a NODE_MODULE_VERSION mismatch at startup.
    substituteInPlace $out/.claude-plugin/plugin.json \
      --replace-fail '"command": "node"' '"command": "${nodejs}/bin/node"'

    # Pin the shebang to the exact Node.js used to compile better-sqlite3.
    # start.mjs uses #!/usr/bin/env node, which resolves whatever node is on
    # PATH at runtime. If that node differs from the one that compiled the
    # native addon, Node.js raises a NODE_MODULE_VERSION mismatch at startup.
    #
    # Disable the runtime npm-install logic in start.mjs v1.0.75:
    #   - ensure-deps.mjs auto-runs npm install better-sqlite3 on import
    #   - inline loop tries to npm-install turndown/domino/gfm at startup
    # Both are unnecessary (deps are pre-built by Nix) and fail in the
    # read-only Nix store, adding silent latency on every MCP startup.
    substituteInPlace $out/start.mjs \
      --replace-fail '#!/usr/bin/env node' '#!${nodejs}/bin/node' \
      --replace-fail \
        'import "./hooks/ensure-deps.mjs";' \
        '// ensure-deps: disabled (native deps pre-built by Nix)' \
      --replace-fail \
        'for (const pkg of ["turndown", "turndown-plugin-gfm", "@mixmark-io/domino"])' \
        'for (const pkg of [/* deps pre-built by Nix */])'

    # Patch server.bundle.mjs to eliminate synchronous runtime detection.
    # context-mode v1.0.75 probes for 15 runtimes via execSync("command -v <rt>")
    # then runs "<rt> --version" for each found one. On Nix, the available runtimes
    # are fixed at build time, so we pre-compute the result.
    #
    # fa()  — runtime availability map (was xa() in v1.0.54, da() in v1.0.25)
    # Tt()  — version string display  (was Et() in v1.0.54)
    # dx()  — bun availability check  (was ux() in v1.0.54, lp() in v1.0.25)
    #
    # These patches target the minified bundle from context-mode v1.0.75.
    # The test in tests/context-mode-deps.nix verifies the patches applied.
    substituteInPlace $out/server.bundle.mjs \
      --replace-fail \
        'function fa(){let e=dx()?cC():null;return{javascript:e??process.execPath,typescript:e||(je("tsx")?"tsx":je("ts-node")?"ts-node":null),python:je("python3")?"python3":je("python")?"python":null,shell:pp?uC()??(je("sh")?"sh":je("powershell")?"powershell":"cmd.exe"):je("bash")?"bash":"sh",ruby:je("ruby")?"ruby":null,go:je("go")?"go":null,rust:je("rustc")?"rustc":null,php:je("php")?"php":null,perl:je("perl")?"perl":null,r:je("Rscript")?"Rscript":je("r")?"r":null,elixir:je("elixir")?"elixir":null}}' \
        'function fa(){return{javascript:"node",typescript:null,python:"python3",shell:"bash",ruby:null,go:null,rust:null,php:null,perl:null,r:null,elixir:null}}' \
      --replace-fail \
        'function Tt(t){try{return dp(`''${t} --version`,{encoding:"utf-8",stdio:["pipe","pipe","pipe"],timeout:5e3}).trim().split(/\r?\n/)[0]}catch{return"unknown"}}' \
        'function Tt(t){return"available"}' \
      --replace-fail \
        'function dx(){if(je("bun"))return!0;if(!pp){let t=process.env.HOME??process.env.USERPROFILE??"";if(t&&lx(`''${t}/.bun/bin/bun`))return!0}return!1}' \
        'function dx(){return false}'
  ''
