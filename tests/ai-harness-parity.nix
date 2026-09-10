{pkgs}: let
  inherit (pkgs) lib;
  renderers = import ../modules/home-manager/programs/ai/renderers.nix {inherit lib;};
  inherit
    (renderers)
    renderAntigravity
    renderCodex
    renderOmp
    renderOpenCode
    renderRulesContext
    renderSkills
    ;

  assertEq = name: actual: expected:
    if actual == expected
    then true
    else
      builtins.throw
      "parity test '${name}' failed: expected ${builtins.toJSON expected}, got ${builtins.toJSON actual}";

  # Force the expression deeply so lazily-built attrsets cannot hide a throw.
  assertThrows = name: expr: let
    result = builtins.tryEval (builtins.deepSeq expr expr);
  in
    if result.success
    then builtins.throw "parity test '${name}' failed: expected an eval-time error"
    else true;

  # Module-shaped fixtures: every option present, transport-irrelevant options
  # left at their defaults, exactly as `programs.ai.manifest` produces.
  stdioFull = {
    transport = "stdio";
    command = "/nix/store/stdio-full/bin/server";
    args = ["--verbose" "--port" "9000"];
    env = {TOKEN = "secret";};
    url = null;
    headers = {};
    oauth = null;
  };
  stdioBare = {
    transport = "stdio";
    command = "/nix/store/stdio-bare/bin/server";
  };
  httpFull = {
    transport = "http";
    command = null;
    args = [];
    env = {};
    url = "https://full.test/mcp";
    headers = {Authorization = "Bearer token";};
    oauth = {
      clientId = "1601185624273.8899143856786";
      callbackPort = 3118;
    };
  };
  httpBare = {
    transport = "http";
    url = "https://bare.test/mcp";
  };

  servers = {
    full-stdio = stdioFull;
    bare-stdio = stdioBare;
    full-http = httpFull;
    bare-http = httpBare;
  };

  portableServers = {
    bare-stdio = stdioBare;
    bare-http = httpBare;
  };
  moduleManifest =
    (lib.evalModules {
      modules = [
        ../modules/home-manager/programs/ai
        {
          programs.ai.manifest.mcpServers.example = {
            transport = "stdio";
            command = "module-mcp";
            args = ["--module"];
          };
        }
      ];
    }).config.programs.ai.manifest.mcpServers;

  test-render-codex = assert assertEq "codex stdio full" (renderCodex {full = stdioFull;}) {
    full = {
      command = "/nix/store/stdio-full/bin/server";
      args = ["--verbose" "--port" "9000"];
      env = {TOKEN = "secret";};
    };
  };
  assert assertEq "codex stdio bare" (renderCodex {bare = stdioBare;}) {
    bare = {command = "/nix/store/stdio-bare/bin/server";};
  };
  assert assertEq "codex http bare" (renderCodex {bare = httpBare;}) {
    bare = {url = "https://bare.test/mcp";};
  }; true;

  test-render-antigravity = assert assertEq "antigravity stdio full" (renderAntigravity {full = stdioFull;}) {
    full = {
      command = "/nix/store/stdio-full/bin/server";
      args = ["--verbose" "--port" "9000"];
      env = {TOKEN = "secret";};
    };
  };
  assert assertEq "antigravity http bare" (renderAntigravity {bare = httpBare;}) {
    bare = {httpUrl = "https://bare.test/mcp";};
  }; true;

  test-render-opencode = assert assertEq "opencode stdio args" (renderOpenCode {withArgs = stdioFull // {env = {};};}) {
    withArgs = {
      type = "local";
      command = ["/nix/store/stdio-full/bin/server" "--verbose" "--port" "9000"];
      enabled = true;
    };
  };
  assert assertEq "opencode stdio bare" (renderOpenCode {bare = stdioBare;}) {
    bare = {
      type = "local";
      command = ["/nix/store/stdio-bare/bin/server"];
      enabled = true;
    };
  };
  assert assertEq "opencode http bare" (renderOpenCode {bare = httpBare;}) {
    bare = {
      type = "remote";
      url = "https://bare.test/mcp";
      enabled = true;
    };
  }; true;

  test-render-omp = assert assertEq "omp stdio full" (renderOmp {full = stdioFull;}) {
    full = {
      command = "/nix/store/stdio-full/bin/server";
      args = ["--verbose" "--port" "9000"];
      env = {TOKEN = "secret";};
    };
  };
  assert assertEq "omp http full" (renderOmp {full = httpFull;}) {
    full = {
      type = "http";
      url = "https://full.test/mcp";
      headers = {Authorization = "Bearer token";};
      oauth = {
        clientId = "1601185624273.8899143856786";
        callbackPort = 3118;
      };
    };
  };
  assert assertEq "omp http bare" (renderOmp {bare = httpBare;}) {
    bare = {
      type = "http";
      url = "https://bare.test/mcp";
    };
  }; true;

  # Every renderer must publish exactly the declared server names.
  test-name-sets = assert assertEq "codex name set" (builtins.attrNames (renderCodex portableServers)) (builtins.attrNames portableServers);
  assert assertEq "antigravity name set" (builtins.attrNames (renderAntigravity portableServers)) (builtins.attrNames portableServers);
  assert assertEq "opencode name set" (builtins.attrNames (renderOpenCode portableServers)) (builtins.attrNames portableServers);
  assert assertEq "omp name set" (builtins.attrNames (renderOmp portableServers)) (builtins.attrNames portableServers); true;
  test-module-renderer = assert assertEq "module manifest renderer" (renderCodex moduleManifest) {
    example = {
      command = "module-mcp";
      args = ["--module"];
    };
  }; true;

  ruleA = builtins.toFile "ai-parity-rule-a.md" "alpha rule";
  ruleB = builtins.toFile "ai-parity-rule-b.md" "beta rule";

  test-rules = assert assertEq "rules ordered" (renderRulesContext [ruleA ruleB]) "alpha rule\n\nbeta rule";
  assert assertEq "rules reversed" (renderRulesContext [ruleB ruleA]) "beta rule\n\nalpha rule";
  assert assertEq "rules empty" (renderRulesContext []) ""; true;

  test-skills = assert renderSkills null == null;
  assert assertEq "skills path" (renderSkills ../ai/skills) ../ai/skills; true;

  # Non-empty fields a target cannot represent must fail, never disappear.
  test-codex-unsupported = assert assertThrows "codex http headers" (renderCodex {server = httpFull;});
  assert assertThrows "codex http oauth" (renderCodex {server = httpBare // {oauth = {clientId = "x";};};});
  assert assertThrows "codex stdio headers" (renderCodex {server = stdioBare // {headers = {A = "b";};};});
  assert assertThrows "codex unknown field" (renderCodex {server = stdioBare // {type = "stdio";};}); true;

  test-antigravity-unsupported = assert assertThrows "antigravity http headers" (renderAntigravity {server = httpFull;});
  assert assertThrows "antigravity http oauth" (renderAntigravity {server = httpBare // {oauth = {clientId = "x";};};});
  assert assertThrows "antigravity stdio headers" (renderAntigravity {server = stdioBare // {headers = {A = "b";};};}); true;

  test-opencode-unsupported = assert assertThrows "opencode http headers" (renderOpenCode {server = httpFull;});
  assert assertThrows "opencode http oauth" (renderOpenCode {server = httpBare // {oauth = {clientId = "x";};};});
  # OpenCode's local mapping has no slot for stdio environment.
  assert assertThrows "opencode stdio env" (renderOpenCode {server = stdioFull;});
  assert assertThrows "opencode http command" (renderOpenCode {server = httpBare // {command = "/bin/server";};}); true;

  test-omp-preserves =
    # OMP keeps everything the manifest can express; nothing to reject here.
    assert assertEq "omp stdio env" (renderOmp {server = stdioFull;}).server.env {TOKEN = "secret";};
    assert assertEq "omp http headers" (renderOmp {server = httpFull;}).server.headers {Authorization = "Bearer token";};
    assert assertEq "omp http oauth" (renderOmp {server = httpFull;}).server.oauth httpFull.oauth; true;
  test-omp-unsupported = assert assertThrows "omp unknown field" (renderOmp {server = stdioBare // {unknown = "unexpected";};}); true;

  test-malformed-transport = assert assertThrows "codex invalid transport" (renderCodex {
    server = {
      transport = "sse";
      command = "/bin/server";
    };
  });
  assert assertThrows "codex missing transport" (renderCodex {server = {command = "/bin/server";};});
  assert assertThrows "codex stdio missing command" (renderCodex {server = {transport = "stdio";};});
  assert assertThrows "codex http missing url" (renderCodex {server = {transport = "http";};});
  assert assertThrows "opencode stdio missing command" (renderOpenCode {server = {transport = "stdio";};});
  assert assertThrows "omp http missing url" (renderOmp {
    server = {
      transport = "http";
      oauth = {clientId = "x";};
    };
  }); true;

  test-mixed-manifest =
    # Realistic manifest: full HTTP metadata that only OMP can carry, plus
    # portable stdio/http entries all four harnesses accept.
    assert assertThrows "mixed codex rejects headers" (renderCodex servers);
    assert assertThrows "mixed antigravity rejects headers" (renderAntigravity servers);
    assert assertThrows "mixed opencode rejects headers" (renderOpenCode servers);
    assert assertEq "mixed omp name set" (builtins.attrNames (renderOmp servers)) (builtins.attrNames servers); true;
in
  assert test-render-codex;
  assert test-render-antigravity;
  assert test-render-opencode;
  assert test-render-omp;
  assert test-name-sets;
  assert test-rules;
  assert test-skills;
  assert test-codex-unsupported;
  assert test-antigravity-unsupported;
  assert test-opencode-unsupported;
  assert test-omp-preserves;
  assert test-malformed-transport;
  assert test-mixed-manifest;
  assert test-omp-unsupported;
  assert test-module-renderer; "all parity tests passed"
