{
  pkgs,
  user,
  lib,
  config,
  ...
}: let
  aws-console = pkgs.callPackage ../../pkgs/aws-console {};
  aws-saml = pkgs.callPackage ../../pkgs/aws-saml {};
  auto-merge-previously-reviewed-api-docs-prs = pkgs.callPackage ../../pkgs/auto-merge-previously-reviewed-api-docs-prs {};
  auto-merge-prs-that-only-bump-openapi-spec-version-numbers = pkgs.callPackage ../../pkgs/auto-merge-prs-that-only-bump-openapi-spec-version-numbers {};
  litellm = pkgs.callPackage ../../pkgs/litellm {};
in {
  home = {
    packages = with pkgs; [
      amazon-ecr-credential-helper
      audacity
      auto-merge-previously-reviewed-api-docs-prs
      aws-console
      aws-saml
      awscli2
      dive
      element-desktop
      fabric-ai
      go-jira
      k9s
      kubectl
      kubectx
      kustomize
      litellm
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
           export ANTHROPIC_MODEL="arn:aws:bedrock:us-west-2::foundation-model/anthropic.claude-3-7-sonnet-20250219-v1:0"
           export AWS_PROFILE="dev"
           export AWS_REGION="us-west-2"
           export CLAUDE_CODE_USE_BEDROCK="1"
           export DISABLE_PROMPT_CACHING="1"

          exec ~/.local/share/npm/bin/claude "$@"
        '';
      };
    };

    # Add ~/.local/bin to PATH
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

      updateClaudeConfig = config.lib.dag.entryAfter ["writeBoundary"] ''
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
        # NOTE: These are commented out because I'm not sure if they're necessary.
        # Create necessary directories for LibreChat
        # run mkdir -p "$HOME/.config/librechat/images"
        # run mkdir -p "$HOME/.config/librechat/uploads"
        # run mkdir -p "$HOME/.config/librechat/logs"
        # run mkdir -p "$HOME/.config/librechat/data-node"
        # run mkdir -p "$HOME/.config/librechat/meili_data_v1.12"

        # Create a real .env file (not a symlink) that Docker can use
        if [ -f "$HOME/.config/librechat/.env" ]; then
          run install -m644 "$HOME/.config/librechat/.env" "$HOME/.config/librechat/.env.docker"
        fi

        # Do the same for librechat.yaml
        if [ -f "$HOME/.config/librechat/librechat.yaml" ]; then
          run install -m644 "$HOME/.config/librechat/librechat.yaml" "$HOME/.config/librechat/librechat.yaml.docker"
        fi
      '';

      installClaudeCode = config.lib.dag.entryAfter ["writeBoundary"] ''
        # Check if claude binary is already installed
        if [ ! -f "${config.home.homeDirectory}/.local/share/npm/bin/claude" ]; then
          run echo "Installing claude-code via npm..."
          # Set PATH to include nodejs bin directory so that 'node' is available during npm install
          PATH="${pkgs.nodejs}/bin:$PATH" run ${pkgs.nodejs}/bin/npm install --global @anthropic-ai/claude-code@0.2.27
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

  launchd = {
    agents = {
      auto-merge-prs-that-only-bump-openapi-spec-version-numbers = {
        enable = true;
        config = {
          ProgramArguments = [
            "${auto-merge-prs-that-only-bump-openapi-spec-version-numbers}/bin/auto-merge-prs-that-only-bump-openapi-spec-version-numbers"
          ];
          EnvironmentVariables = {
            GITHUB_TOKEN_PATH = "${config.sops.secrets.github_token_bitgo.path}";
          };
          StartInterval = 300; # every 5 minutes
          RunAtLoad = true;
          StandardOutPath = "${user.homeDirectory}/Library/Logs/auto-merge-prs-that-only-bump-openapi-spec-version-numbers.log";
          StandardErrorPath = "${user.homeDirectory}/Library/Logs/auto-merge-prs-that-only-bump-openapi-spec-version-numbers.error.log";
          ServiceDependencies = ["sops-nix"];
        };
      };
      auto-merge-previously-reviewed-api-docs-prs = {
        enable = true;
        config = {
          ProgramArguments = [
            "${auto-merge-previously-reviewed-api-docs-prs}/bin/auto-merge-previously-reviewed-api-docs-prs"
          ];
          EnvironmentVariables = {
            GITHUB_TOKEN_PATH = "${config.sops.secrets.github_token_bitgo.path}";
          };
          StartInterval = 300; # every 5 minutes
          RunAtLoad = true;
          StandardOutPath = "${user.homeDirectory}/Library/Logs/auto-merge-previously-reviewed-api-docs-prs.log";
          StandardErrorPath = "${user.homeDirectory}/Library/Logs/auto-merge-previously-reviewed-api-docs-prs.error.log";
          ServiceDependencies = ["sops-nix"];
        };
      };
      litellm-proxy = {
        enable = true;
        config = {
          ProgramArguments = [
            "${litellm}/bin/litellm"
            "--config"
            "/Users/ericcrosson/.config/litellm/config.yaml"
          ];
          EnvironmentVariables = {
            PATH = lib.makeBinPath [aws-saml];
          };
          KeepAlive = true;
          RunAtLoad = true;
          StandardOutPath = "${user.homeDirectory}/Library/Logs/litellm-proxy.log";
          StandardErrorPath = "${user.homeDirectory}/Library/Logs/litellm-proxy.error.log";
        };
      };
      colima = {
        enable = true;
        config = {
          ProgramArguments = [
            "/opt/homebrew/bin/colima"
            "start"
            "--cpu"
            "8"
            "--memory"
            "8"
            "--arch"
            "aarch64"
            "--vm-type=vz"
            "--vz-rosetta"
          ];
          EnvironmentVariables = {
            PATH = "/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin";
          };
          RunAtLoad = true;
          KeepAlive = false; # Only run once at startup
          StandardOutPath = "${user.homeDirectory}/Library/Logs/colima.log";
          StandardErrorPath = "${user.homeDirectory}/Library/Logs/colima.error.log";
        };
      };
      librechat = {
        enable = true;
        config = {
          ProgramArguments = [
            "/opt/homebrew/bin/docker-compose"
            "-f"
            "${user.homeDirectory}/.config/librechat/docker-compose.yml"
            "-f"
            "${user.homeDirectory}/.config/librechat/docker-compose.override.yml"
            "up"
            "-d"
          ];
          WorkingDirectory = "${user.homeDirectory}/.config/librechat";
          EnvironmentVariables = {
            PATH = "/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin";
          };
          KeepAlive = true;
          RunAtLoad = true;
          StandardOutPath = "${user.homeDirectory}/Library/Logs/librechat.log";
          StandardErrorPath = "${user.homeDirectory}/Library/Logs/librechat.error.log";
          # Make sure colima is started first
          ServiceDependencies = ["org.nixos.home-manager.colima"];
        };
      };
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
