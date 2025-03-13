{
  pkgs,
  user,
  config,
  ...
}: {
  imports = [
    ../../modules/home-manager/launchd-with-logs.nix
    ./darwin-services.nix
  ];

  home = {
    packages = with pkgs; [
      amazon-ecr-credential-helper
      audacity
      (callPackage ../../pkgs/aws-console {})
      (callPackage ../../pkgs/aws-saml {})
      awscli2
      dive
      element-desktop
      fabric-ai
      go-jira
      k9s
      kubectl
      kubectx
      kustomize
      openai-whisper
      yq-go
      nodejs # Ensure nodejs is installed for npm
    ];

    file = {
      ".aider.conf.yml" = {
        source = ../../.aider.conf.yml;
      };
      ".config/litellm/config.yaml" = {
        source = ../../.config/litellm/config.yaml;
      };
      ".config/fabric/.env" = {
        source = ../../.config/fabric/.env;
      };
      ".config/fabric/config.yaml" = {
        source = ../../.config/fabric/config.yaml;
      };
      ".config/librechat/docker-compose.yml" = {
        source = ../../.config/librechat/docker-compose.yml;
      };
      ".config/librechat/docker-compose.override.yml" = {
        source = ../../.config/librechat/docker-compose.override.yml;
      };
      ".jira.d" = {
        # I would prefer this to be true but that doesn't appear to be working right now
        recursive = false;
        source = ../../.jira.d;
      };
      ".npmrc" = {
        text = ''
          prefix=${config.home.homeDirectory}/.local/share/npm
        '';
      };
      # NOTE: this is a darwin-specific path, will need to be modified on Linux
      "Library/Application Support/io.datasette.llm/default_model.txt" = {
        source = ../.. + "/Library/Application Support/io.datasette.llm/default_model.txt";
      };
      # NOTE: this is a darwin-specific path, will need to be modified on Linux
      "Library/Application Support/io.datasette.llm/extra-openai-models.yaml" = {
        source = ../.. + "/Library/Application Support/io.datasette.llm/extra-openai-models.yaml";
      };

      ".ssh/id_rsa_personal.pub".source = ../../.ssh/id_rsa_personal.pub;

      # Create a wrapper script for claude-code
      ".local/bin/claude" = {
        executable = true;
        text = ''
           #!/bin/sh
           export ANTHROPIC_MODEL="arn:aws:bedrock:us-west-2:319156457634:inference-profile%2Fus.anthropic.claude-3-7-sonnet-20250219-v1:0"
           export AWS_PROFILE="dev"
           export AWS_REGION="us-west-2"
           export CLAUDE_CODE_USE_BEDROCK="1"
           export DISABLE_PROMPT_CACHING="1"

          exec ~/.local/share/npm/bin/claude "$@"
        '';
      };
    };

    sessionPath = [
      "$HOME/.local/bin"
      "$HOME/.local/share/npm/bin"
    ];

    activation = {
      copySSHKey = config.lib.dag.entryAfter ["writeBoundary"] ''
        if [ -f "${config.sops.secrets.github_ssh_private_key_personal.path}" ]; then
          run install -m600 "${config.sops.secrets.github_ssh_private_key_personal.path}" "${config.home.homeDirectory}/.ssh/id_rsa_personal"
        fi
      '';

      configureClaudeConfig = config.lib.dag.entryAfter ["writeBoundary"] ''
        CLAUDE_CONFIG="${config.home.homeDirectory}/.claude.json"

        if [ ! -f "$CLAUDE_CONFIG" ]; then
          # Create the file if it doesn't exist
          run echo '{}' > "$CLAUDE_CONFIG"
        fi

        # Use jq to ensure the keys are set with the specified values
        run ${pkgs.jq}/bin/jq '.preferredNotifChannel = "terminal_bell" | .autoUpdaterStatus = "disabled"' "$CLAUDE_CONFIG" > "$CLAUDE_CONFIG.tmp"
        run mv "$CLAUDE_CONFIG.tmp" "$CLAUDE_CONFIG"
      '';

      createLibreChatDirs = config.lib.dag.entryAfter ["writeBoundary"] ''
        # Create a real .env file (not a symlink) that Docker can use
        if [ ! -f "${config.home.homeDirectory}/.config/librechat/.env" ]; then
          run install -m644 "${../../.config/librechat/.env}" "${config.home.homeDirectory}/.config/librechat/.env"
        fi

        # Do the same for librechat.yaml
        if [ ! -f "${config.home.homeDirectory}/.config/librechat/librechat.yaml" ]; then
          run install -m644 "${../../.config/librechat/librechat.yaml}" "${config.home.homeDirectory}/.config/librechat/librechat.yaml"
        fi
      '';

      installClaudeCode = config.lib.dag.entryAfter ["writeBoundary"] ''
        # Check if claude binary is already installed
        if [ ! -f "${config.home.homeDirectory}/.local/share/npm/bin/claude" ]; then
          run echo "Installing claude-code via npm..."
          # Set PATH to include nodejs bin directory so that 'node' is available during npm install
          PATH="${pkgs.nodejs}/bin:$PATH" run ${pkgs.nodejs}/bin/npm install --global @anthropic-ai/claude-code@0.2.39
        fi
      '';
    };
  };

  programs = {
    git = {
      signing = {
        key = "5BD755D7FD4AFCB6";
        signByDefault = true;
      };
    };

    zsh = {
      envExtra = builtins.readFile ../../zsh/bitgo_zshenv.zsh;
      initExtra = builtins.readFile ../../zsh/bitgo_zshrc.zsh;
    };
  };

  sops = {
    defaultSopsFile = ../../secrets/main.yaml;
    gnupg.home = user.homeDirectory + "/.gnupg";
    secrets = {
      github_ssh_private_key_personal = {};
      github_token_bitgo = {};
      github_token_personal = {};
      jira_token_bitgo = {};
      youtube_api_key = {};
    };
  };
}
