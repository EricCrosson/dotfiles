{
  lib,
  buildGoModule,
  writeShellApplication,
  jq,
  curl,
  _1password-cli,
  bash,
}: {
  bedrockProfile,
  bedrockRegion,
  bedrockThreshold ? 80,
  bedrockWeeklyThreshold ? 65,
  envTemplate,
  extraPathPackages ? [],
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
    runtimeInputs = [claude-wrapper-go jq curl _1password-cli bash];
    text = ''
      # Nix-injected configuration
      export AWS_PROFILE=${lib.escapeShellArg bedrockProfile}
      export AWS_REGION=${lib.escapeShellArg bedrockRegion}
      export BEDROCK_THRESHOLD=${toString bedrockThreshold}
      export BEDROCK_WEEKLY_THRESHOLD=${toString bedrockWeeklyThreshold}
      export ENV_TEMPLATE=${lib.escapeShellArg envTemplate}
      ${lib.concatMapStringsSep "\n      " (pkg: "export PATH=\"${pkg}/bin:$PATH\"") extraPathPackages}

      # Execute the Go wrapper
      exec claude-wrapper "$@"
    '';
  }
