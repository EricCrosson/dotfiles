{
  lib,
  buildGoModule,
  writeShellApplication,
  jq,
  curl,
}: {
  claude-code,
  bedrockProfile,
  bedrockRegion,
  bedrockThreshold ? 80,
  bedrockWeeklyThreshold ? 65,
  runtimeInputs ? [],
}: let
  # Build the Go wrapper binary
  claude-wrapper-go = buildGoModule {
    pname = "claude-wrapper";
    version = "2.0.0";
    src = ./.;
    vendorHash = null; # No dependencies

    meta = {
      description = "Claude Code wrapper with Bedrock routing and automatic model selection";
      mainProgram = "claude-wrapper";
    };
  };
in
  writeShellApplication {
    name = "claude";
    excludeShellChecks = ["SC2016"];
    runtimeInputs = [claude-wrapper-go jq curl] ++ runtimeInputs;
    text = ''
      # Nix-injected configuration
      export _CLAUDE_UNWRAPPED=${claude-code}/bin/claude
      export AWS_PROFILE=${lib.escapeShellArg bedrockProfile}
      export AWS_REGION=${lib.escapeShellArg bedrockRegion}
      export BEDROCK_THRESHOLD=${toString bedrockThreshold}
      export BEDROCK_WEEKLY_THRESHOLD=${toString bedrockWeeklyThreshold}

      # Execute the Go wrapper
      exec claude-wrapper "$@"
    '';
  }
