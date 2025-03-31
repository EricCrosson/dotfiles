{
  pkgs,
  user,
  config,
  ...
}: {
  imports = [
    ../../modules/home-manager
  ];

  home = {
    packages = with pkgs; [
      amazon-ecr-credential-helper
      audacity
      (callPackage ../../pkgs/aws-console {})
      (callPackage ../../pkgs/aws-saml {})
      (callPackage ../../pkgs/yt-summarize {})
      awscli2
      dive
      element-desktop
      (pkgs.symlinkJoin {
        name = "go-jirarenamed";
        paths = [pkgs.go-jira];
        postBuild = ''
          rm $out/bin/jira
          ln -s ${pkgs.go-jira}/bin/jira $out/bin/j
        '';
      })
      jira-cli-go
      k9s
      kubectl
      kubectx
      kustomize
      obsidian
      openai-whisper
      yq-go
      nodejs # Ensure nodejs is installed for npm
    ];

    file = {
      ".config/.jira/.config.yml" = {
        source = ../../.config/.jira/.config.yml;
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
    };

    sessionVariables = {
      GITHUB_TOKEN = "$(cat ${config.sops.secrets.github_token_bitgo.path} 2>/dev/null || echo '')";
      GITHUB_TOKEN_PERSONAL = "$(cat  ${config.sops.secrets.github_token_personal.path} 2>/dev/null || echo '')";
      JIRA_API_TOKEN = "$(cat ${config.sops.secrets.jira_token_bitgo.path} 2>/dev/null || echo '')";
      JIRA_USERNAME = "ericcrosson@bitgo.com";
      YOUTUBE_API_KEY = "$(cat ${config.sops.secrets.youtube_api_key.path} 2>/dev/null || echo '')";
      NIX_CONFIG = "access-tokens = github.com=\${GITHUB_TOKEN}";
    };

    activation = {
      copySSHKey = config.lib.dag.entryAfter ["writeBoundary"] ''
        if [ -f "${config.sops.secrets.github_ssh_private_key_personal.path}" ]; then
          run install -m600 "${config.sops.secrets.github_ssh_private_key_personal.path}" "${config.home.homeDirectory}/.ssh/id_rsa_personal"
        fi
      '';
    };
  };

  programs = {
    aichat = {
      enable = true;
      keybindings = "emacs";
      wrap = "auto";
      clients = [
        {
          type = "openai-compatible";
          name = "bedrock-claude";
          api_base = "http://localhost:4000/v1";
          api_key = "xxx";
          models = [
            {
              name = "bedrock-claude-sonnet";
              max_input_tokens = 200000;
              supports_function_calling = false;
              supports_vision = false;
            }
          ];
        }
      ];
    };

    aider = {
      enable = true;
      codeTheme = "lightbulb";
      detectUrls = false;
      gitignore = false;
      subtreeOnly = true;
      watchFiles = true;
      read = ["CONVENTIONS.md"];
      extraEnv = {
        SMART_CD_GIT_STATUS = "false";
        SMART_CD_LS = "false";
      };
    };

    claude-code = {
      enable = true;
    };

    fabric = {
      enable = true;
      env = {
        # Configured to proxy through litellm to Amazon Bedrock
        DEFAULT_VENDOR = "OpenAI";
        DEFAULT_MODEL = "bedrock-claude-sonnet";
        DEFAULT_MODEL_CONTEXT_LENGTH = "200000";
        OPENAI_API_KEY = "has-to-be-populated-but-this-is-definitely-not-a-secret";
        OPENAI_API_BASE_URL = "http://localhost:4000";
      };
    };

    git = {
      signing = {
        key = "5BD755D7FD4AFCB6";
        signByDefault = true;
      };
    };

    zsh = {
      shellAliases = {
        chat = "aichat";
        cmd = "aichat -e";
      };
    };
  };

  services = {
    # Enable auto-merge services for BitGo PRs
    auto-merge-bitgo-prs = {
      previouslyReviewedOpenapiSpecs.enable = true;
      trivialOpenapiSpecVersionBump.enable = true;
    };

    # Enable keychain for SSH key management
    keychain = {
      enable = true;
      keys = [
        "${user.homeDirectory}/.ssh/id_rsa"
        "${user.homeDirectory}/.ssh/id_rsa_personal"
      ];
    };

    # Enable LibreChat service
    librechat.enable = true;

    # Enable LiteLLM proxy for AI models
    litellm-proxy = {
      enable = true;
      configFile = ../../.config/litellm/config.yaml;
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
