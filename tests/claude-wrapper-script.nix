{pkgs}: let
  inherit (pkgs) lib;
  helpers = import ./helpers.nix {inherit lib;};
  inherit (helpers) assertContains;

  # Build the claude wrapper with mock inputs
  wrapper = pkgs.callPackage ../pkgs/claude-wrapper {} {
    claude-code = pkgs.hello;
    bedrockProfile = "test-profile";
    bedrockRegion = "us-test-1";
    bedrockOpusFile = "/mock/opus-arn";
    bedrockSonnetFile = "/mock/sonnet-arn";
    bedrockHaikuFile = "/mock/haiku-arn";
  };

  # Read the generated shell script via IFD
  script = builtins.readFile "${wrapper}/bin/claude";

  # === Contract assertions ===

  # The Nix shell wrapper must set _CLAUDE_UNWRAPPED so the Go binary
  # knows which claude binary to exec
  test-unwrapped-var = assert assertContains "unwrapped-set" script "_CLAUDE_UNWRAPPED="; true;

  # AWS_PROFILE defaults to the configured Bedrock profile
  test-aws-profile = assert assertContains "aws-profile-default" script "test-profile"; true;

  # AWS_REGION defaults to the configured Bedrock region
  test-aws-region = assert assertContains "aws-region-default" script "us-test-1"; true;

  # ANTHROPIC_MODEL is set (reads from opus file by default)
  test-anthropic-model = assert assertContains "anthropic-model" script "ANTHROPIC_MODEL"; true;

  # Per-tier model env vars reference the correct sops file paths
  test-opus-model = assert assertContains "opus-model-var" script "ANTHROPIC_DEFAULT_OPUS_MODEL";
  assert assertContains "opus-file-ref" script "/mock/opus-arn"; true;

  test-sonnet-model = assert assertContains "sonnet-model-var" script "ANTHROPIC_DEFAULT_SONNET_MODEL";
  assert assertContains "sonnet-file-ref" script "/mock/sonnet-arn"; true;

  test-haiku-model = assert assertContains "haiku-model-var" script "ANTHROPIC_DEFAULT_HAIKU_MODEL";
  assert assertContains "haiku-file-ref" script "/mock/haiku-arn"; true;

  # _CLAUDE_AVAILABLE_MODELS tells the Go binary which models to advertise
  test-available-models = assert assertContains "available-models" script "_CLAUDE_AVAILABLE_MODELS='opus,sonnet,haiku'"; true;

  # The shell wrapper must exec the Go binary, not some other command
  test-exec = assert assertContains "exec-wrapper" script "exec claude-wrapper"; true;
in
  assert test-unwrapped-var;
  assert test-aws-profile;
  assert test-aws-region;
  assert test-anthropic-model;
  assert test-opus-model;
  assert test-sonnet-model;
  assert test-haiku-model;
  assert test-available-models;
  assert test-exec; "all tests passed"
