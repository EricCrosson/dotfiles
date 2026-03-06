{
  lib,
  buildGoModule,
  writeShellApplication,
}: {
  claude-code,
  bedrockProfile,
  bedrockRegion,
  bedrockModelFile, # path to sops-decrypted file containing model ARN
}: let
  # Build the Go wrapper binary
  claude-wrapper-go = buildGoModule {
    pname = "claude-wrapper";
    version = "3.0.0";
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
    runtimeInputs = [claude-wrapper-go];
    text = ''
      # Nix-injected configuration; the ''$ escape produces a literal $
      # for shell parameter expansion. Nix interpolation fills in defaults.
      export _CLAUDE_UNWRAPPED=${claude-code}/bin/claude
      export AWS_PROFILE=''${AWS_PROFILE:-${lib.escapeShellArg bedrockProfile}}
      export AWS_REGION=''${AWS_REGION:-${lib.escapeShellArg bedrockRegion}}

      if [ ! -f ${lib.escapeShellArg bedrockModelFile} ]; then
        echo "Error: Bedrock model file not found at ${lib.escapeShellArg bedrockModelFile}" >&2
        echo "Run 'darwin-rebuild switch' to decrypt sops secrets." >&2
        exit 1
      fi
      export ANTHROPIC_MODEL=''${ANTHROPIC_MODEL:-$(cat ${lib.escapeShellArg bedrockModelFile})}

      # Execute the Go wrapper
      exec claude-wrapper "$@"
    '';
  }
