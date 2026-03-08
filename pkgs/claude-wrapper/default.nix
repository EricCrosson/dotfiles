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
    version = "5.0.0";
    src = ./.;
    vendorHash = null; # No dependencies

    meta = {
      description = "Claude Code wrapper with optional Bedrock routing";
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

      # Bedrock config — available for the Go wrapper when --bedrock is passed.
      # Paths are exported; the Go wrapper reads files on demand.
      export _CLAUDE_BEDROCK_PROFILE=''${_CLAUDE_BEDROCK_PROFILE:-${lib.escapeShellArg bedrockProfile}}
      export _CLAUDE_BEDROCK_REGION=''${_CLAUDE_BEDROCK_REGION:-${lib.escapeShellArg bedrockRegion}}
      export _CLAUDE_BEDROCK_OPUS_FILE=${lib.escapeShellArg bedrockOpusFile}
      export _CLAUDE_BEDROCK_SONNET_FILE=${lib.escapeShellArg bedrockSonnetFile}
      export _CLAUDE_BEDROCK_HAIKU_FILE=${lib.escapeShellArg bedrockHaikuFile}

      # Execute the Go wrapper
      exec claude-wrapper "$@"
    '';
  }
