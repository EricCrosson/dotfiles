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
      default = "0.2.38";
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
    home = {
      # Create claude wrapper without adding nodejs to user's path
      packages = [
        # Create a properly wrapped executable using symlinkJoin and makeWrapper
        (pkgs.symlinkJoin {
          name = "claude-code-wrapped";
          paths = []; # We're creating a wrapper, not including other packages
          buildInputs = [pkgs.makeWrapper];

          # After the symlink join build phase, we wrap the executable
          postBuild = ''
            mkdir -p $out/bin

            # Create the base script
            cat > $out/bin/claude << 'EOF'
            #!/bin/sh
            exec ${config.home.homeDirectory}/.local/share/npm/bin/claude "$@"
            EOF

            chmod +x $out/bin/claude

            # Wrap the script with proper environment variables and PATH
            wrapProgram $out/bin/claude \
              --prefix PATH : ${lib.makeBinPath [pkgs.nodejs]} \
              --set ANTHROPIC_MODEL "${cfg.model}" \
              --set AWS_PROFILE "${cfg.awsProfile}" \
              --set AWS_REGION "${cfg.awsRegion}" \
              --set CLAUDE_CODE_USE_BEDROCK "${lib.boolToString cfg.useBedrock}" \
              --set DISABLE_PROMPT_CACHING "${lib.boolToString cfg.disablePromptCaching}" \
              --set SMART_CD_GIT_STATUS "false" \
              --set SMART_CD_LS "false"
          '';
        })
      ];

      # Ensure the claude config exists and has the right settings
      activation.configureClaudeConfig = lib.hm.dag.entryAfter ["writeBoundary"] ''
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
      activation.installClaudeCode = lib.hm.dag.entryAfter ["writeBoundary"] ''
        # Set up path to use the specific Node.js version from nixpkgs
        NODEJS_BIN="${pkgs.nodejs}/bin"
        NPM="$NODEJS_BIN/npm"

        # Check if the specific version of claude-code is installed using npm itself
        if ! PATH="$NODEJS_BIN:$PATH" $NPM list -g ${cfg.package}@${cfg.version} &>/dev/null; then
          run echo "Installing claude-code via npm..."
          # Use nodejs from Nix store for installation without affecting user's PATH
          PATH="$NODEJS_BIN:$PATH" run $NPM install --global ${cfg.package}@${cfg.version}
        fi
      '';
    };
  };
}
