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
in
  assert test-unwrapped-var;
  assert test-bedrock-profile;
  assert test-bedrock-region;
  assert test-bedrock-opus-file;
  assert test-bedrock-sonnet-file;
  assert test-bedrock-haiku-file;
  assert test-exec; "all tests passed"
