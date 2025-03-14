{
  pkgs,
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.programs.claude-code;
in {
  options.programs.claude-code = {
    enable = mkEnableOption "Claude Code CLI";

    package = mkOption {
      type = types.str;
      default = "@anthropic-ai/claude-code";
      description = "npm package name for Claude Code";
    };

    version = mkOption {
      type = types.str;
      default = "0.2.39";
      description = "Version of Claude Code to install";
    };

    model = mkOption {
      type = types.str;
      default = "arn:aws:bedrock:us-west-2:319156457634:inference-profile%2Fus.anthropic.claude-3-7-sonnet-20250219-v1:0";
      description = "Claude model ARN to use";
    };

    awsProfile = mkOption {
      type = types.str;
      default = "dev";
      description = "AWS profile to use for authentication";
    };

    awsRegion = mkOption {
      type = types.str;
      default = "us-west-2";
      description = "AWS region to use for Bedrock";
    };

    useBedrock = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to use AWS Bedrock for Claude";
    };

    disablePromptCaching = mkOption {
      type = types.bool;
      default = true;
      description = "Whether to disable prompt caching";
    };

    config = {
      preferredNotifChannel = mkOption {
        type = types.str;
        default = "terminal_bell";
        description = "Preferred notification channel";
      };

      autoUpdaterStatus = mkOption {
        type = types.str;
        default = "disabled";
        description = "Auto updater status";
      };
    };
  };

  config = mkIf cfg.enable {
    # Add nodejs to path
    home.packages = [pkgs.nodejs];

    # Create wrapper script
    home.file.".local/bin/claude" = {
      executable = true;
      text = ''
        #!/bin/sh
        export ANTHROPIC_MODEL="${cfg.model}"
        export AWS_PROFILE="${cfg.awsProfile}"
        export AWS_REGION="${cfg.awsRegion}"
        export CLAUDE_CODE_USE_BEDROCK="${lib.boolToString cfg.useBedrock}"
        export DISABLE_PROMPT_CACHING="${lib.boolToString cfg.disablePromptCaching}"

        exec ~/.local/share/npm/bin/claude "$@"
      '';
    };

    # Ensure the claude config exists and has the right settings
    home.activation.configureClaudeConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
      CLAUDE_CONFIG="${config.home.homeDirectory}/.claude.json"

      if [ ! -f "$CLAUDE_CONFIG" ]; then
        # Create the file if it doesn't exist
        run echo '{}' > "$CLAUDE_CONFIG"
      fi

      # Use jq to ensure the keys are set with the specified values
      run ${pkgs.jq}/bin/jq '.preferredNotifChannel = "${cfg.config.preferredNotifChannel}" | .autoUpdaterStatus = "${cfg.config.autoUpdaterStatus}"' "$CLAUDE_CONFIG" > "$CLAUDE_CONFIG.tmp"
      run mv "$CLAUDE_CONFIG.tmp" "$CLAUDE_CONFIG"
    '';

    # Install Claude Code via npm
    home.activation.installClaudeCode = lib.hm.dag.entryAfter ["writeBoundary"] ''
      # Check if claude binary is already installed
      if [ ! -f "${config.home.homeDirectory}/.local/share/npm/bin/claude" ]; then
        run echo "Installing claude-code via npm..."
        # Set PATH to include nodejs bin directory so that 'node' is available during npm install
        PATH="${pkgs.nodejs}/bin:$PATH" run ${pkgs.nodejs}/bin/npm install --global ${cfg.package}@${cfg.version}
      fi
    '';
  };
}
