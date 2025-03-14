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
      awscli2
      dive
      element-desktop
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
    aider = {
      enable = true;
      configFile = ../../.aider.conf.yml;
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
      configFile = ../../.config/fabric/config.yaml;
      envFile = ../../.config/fabric/.env;
    };

    git = {
      signing = {
        key = "5BD755D7FD4AFCB6";
        signByDefault = true;
      };
    };

    zsh = {
      shellAliases = {
        cmd = "llm cmd";
      };
    };
  };

  services = {
    # Enable auto-merge services for BitGo PRs
    auto-merge-bitgo-prs = {
      previouslyReviewedOpenapiSpecs.enable = true;
      trivialOpenapiSpecVersionBump.enable = true;
    };

    # Enable colima for Docker containers
    colima = {
      enable = true;
      cpus = 8;
      memory = 8; # GB
      arch = "aarch64";
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
