{
  lib,
  buildGoModule,
  writeShellApplication,
}: {
  claude-code,
  bedrockProfile,
  bedrockRegion,
  bedrockOpusFile, # path to sops-decrypted file containing opus model ARN
  bedrockSonnetFile, # path to sops-decrypted file containing sonnet model ARN
  bedrockHaikuFile, # path to sops-decrypted file containing haiku model ARN
}: let
  # Build the Go wrapper binary
  claude-wrapper-go = buildGoModule {
    pname = "claude-wrapper";
    version = "4.0.0";
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

      if [ "$AWS_PROFILE" = ${lib.escapeShellArg bedrockProfile} ]; then
        for f in ${lib.escapeShellArg bedrockOpusFile} ${lib.escapeShellArg bedrockSonnetFile} ${lib.escapeShellArg bedrockHaikuFile}; do
          if [ ! -f "$f" ]; then
            echo "Error: Bedrock model file not found at $f" >&2
            echo "Run 'darwin-rebuild switch' to decrypt sops secrets." >&2
            exit 1
          fi
        done
        export ANTHROPIC_MODEL=''${ANTHROPIC_MODEL:-$(cat ${lib.escapeShellArg bedrockOpusFile})}
        export ANTHROPIC_DEFAULT_OPUS_MODEL=''${ANTHROPIC_DEFAULT_OPUS_MODEL:-$(cat ${lib.escapeShellArg bedrockOpusFile})}
        export ANTHROPIC_DEFAULT_SONNET_MODEL=''${ANTHROPIC_DEFAULT_SONNET_MODEL:-$(cat ${lib.escapeShellArg bedrockSonnetFile})}
        export ANTHROPIC_DEFAULT_HAIKU_MODEL=''${ANTHROPIC_DEFAULT_HAIKU_MODEL:-$(cat ${lib.escapeShellArg bedrockHaikuFile})}
        export _CLAUDE_AVAILABLE_MODELS='opus,sonnet,haiku'
      fi

      # Execute the Go wrapper
      exec claude-wrapper "$@"
    '';
  }
