{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertContains assertNotContains;

  mockBedrockArgs = {
    claude-code = pkgs.hello;
    bedrockProfile = "test-profile";
    bedrockRegion = "us-test-1";
    bedrockOpusFile = "/mock/opus-arn";
    bedrockSonnetFile = "/mock/sonnet-arn";
    bedrockHaikuFile = "/mock/haiku-arn";
  };

  # Build the claude wrapper with explicit bedrock default
  wrapper = pkgs.callPackage ../pkgs/claude-wrapper {} (mockBedrockArgs
    // {
      defaultBackend = "bedrock";
    });

  # Build a second wrapper omitting defaultBackend to test the ? "anthropic" default
  wrapperDefault = pkgs.callPackage ../pkgs/claude-wrapper {} mockBedrockArgs;

  # Read the generated shell scripts via IFD
  script = builtins.readFile "${wrapper}/bin/claude";
  scriptDefault = builtins.readFile "${wrapperDefault}/bin/claude";

  # === Contract assertions ===

  # The Nix shell wrapper must set _CLAUDE_UNWRAPPED so the Go binary
  # knows which claude binary to exec
  test-unwrapped-var = assert assertContains "unwrapped-set" script "_CLAUDE_UNWRAPPED="; true;

  # Default backend is exported for the Go wrapper
  test-default-backend = assert assertContains "default-backend" script "_CLAUDE_DEFAULT_BACKEND";
  assert assertContains "default-backend-value" script "bedrock"; true;

  # Omitting defaultBackend produces anthropic as the default
  test-default-backend-fallback = assert assertContains "default-backend-fallback" scriptDefault "_CLAUDE_DEFAULT_BACKEND";
  assert assertContains "default-backend-fallback-value" scriptDefault "anthropic"; true;

  # Bedrock profile defaults to the configured value
  test-bedrock-profile = assert assertContains "bedrock-profile" script "_CLAUDE_BEDROCK_PROFILE";
  assert assertContains "bedrock-profile-value" script "test-profile"; true;

  # Bedrock region defaults to the configured value
  test-bedrock-region = assert assertContains "bedrock-region" script "_CLAUDE_BEDROCK_REGION";
  assert assertContains "bedrock-region-value" script "us-test-1"; true;

  # Sops file paths are exported for the Go wrapper to read on demand
  test-bedrock-opus-file = assert assertContains "opus-file-path" script "_CLAUDE_BEDROCK_OPUS_FILE";
  assert assertContains "opus-file-value" script "/mock/opus-arn"; true;

  test-bedrock-sonnet-file = assert assertContains "sonnet-file-path" script "_CLAUDE_BEDROCK_SONNET_FILE";
  assert assertContains "sonnet-file-value" script "/mock/sonnet-arn"; true;

  test-bedrock-haiku-file = assert assertContains "haiku-file-path" script "_CLAUDE_BEDROCK_HAIKU_FILE";
  assert assertContains "haiku-file-value" script "/mock/haiku-arn"; true;

  # The shell wrapper must exec the Go binary, not some other command
  test-exec = assert assertContains "exec-wrapper" script "exec claude-wrapper"; true;

  # === Plugin directory assertions ===

  # Build a wrapper with plugin directories
  wrapperWithPlugins = pkgs.callPackage ../pkgs/claude-wrapper {} (mockBedrockArgs
    // {
      pluginDirs = ["/mock/plugin-a" "/mock/plugin-b"];
    });
  scriptWithPlugins = builtins.readFile "${wrapperWithPlugins}/bin/claude";

  # Plugin wrapper contains _CLAUDE_PLUGIN_DIRS with both paths colon-joined
  test-plugin-dirs = assert assertContains "plugin-dirs-set" scriptWithPlugins "_CLAUDE_PLUGIN_DIRS";
  assert assertContains "plugin-dirs-paths" scriptWithPlugins "/mock/plugin-a:/mock/plugin-b"; true;

  # Default wrapper (no pluginDirs) does NOT contain _CLAUDE_PLUGIN_DIRS
  test-no-plugin-dirs-default = assert assertNotContains "no-plugin-dirs-default" scriptDefault "_CLAUDE_PLUGIN_DIRS"; true;
in
  assert test-unwrapped-var;
  assert test-default-backend;
  assert test-default-backend-fallback;
  assert test-bedrock-profile;
  assert test-bedrock-region;
  assert test-bedrock-opus-file;
  assert test-bedrock-sonnet-file;
  assert test-bedrock-haiku-file;
  assert test-exec;
  assert test-plugin-dirs;
  assert test-no-plugin-dirs-default; "all tests passed"
