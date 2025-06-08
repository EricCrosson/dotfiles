{
  pkgs,
  profile,
  config,
  inputs,
  ...
}: {
  imports = [
    ../../modules/home-manager
    ../../modules/home-manager/options/services.nix
  ];

  home = {
    packages = with pkgs; [
      amazon-ecr-credential-helper
      audacity
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

      inputs.percentage-changed-calculator.packages.${pkgs.system}.default

      (callPackage ../../pkgs/aws-console {})
      (callPackage ../../pkgs/aws-saml {})
      (callPackage ../../pkgs/yt-summarize {})
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

      ".config/fabric/patterns/write_useful_git_commit_message/readme.md".source = ../../.config/fabric/patterns/write_useful_git_commit_message/readme.md;
      ".config/fabric/patterns/write_useful_git_commit_message/system.md".source = ../../.config/fabric/patterns/write_useful_git_commit_message/system.md;

      ".claude/CLAUDE.md" = {
        source = ../../.claude/CLAUDE.md;
      };

      ".npmrc" = {
        text = ''
          prefix=${config.home.homeDirectory}/.local/share/npm
        '';
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
      ZED_AWS_PROFILE = "dev";
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
      settings = {
        stream = true;
        save = true;
        keybindings = "emacs";
        wrap = "auto";
        save_shell_history = true;
        clients = [
          {
            type = "openai-compatible";
            name = "bedrock-claude";
            api_base = config.services-options.litellm-proxy.apiUrl;
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
      version = (builtins.fromJSON (builtins.readFile "${inputs.claude-code}"))."dist-tags".latest;
    };

    fabric = {
      enable = true;
      env = {
        # Configured to proxy through litellm to Amazon Bedrock
        DEFAULT_VENDOR = "OpenAI";
        DEFAULT_MODEL = config.claude-options.models.default;
        DEFAULT_MODEL_CONTEXT_LENGTH = toString config.claude-options.models.sonnet.contextLength;
        OPENAI_API_KEY = "has-to-be-populated-but-this-is-definitely-not-a-secret";
        OPENAI_API_BASE_URL = config.services-options.litellm-proxy.baseUrl;
      };
    };

    llm = {
      enable = true;
      models = [
        {
          inherit (config.claude-options.models.sonnet) id name;
          api_base = config.services-options.litellm-proxy.baseUrl;
        }
      ];
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
        "${profile.homeDirectory}/.ssh/id_rsa"
        "${profile.homeDirectory}/.ssh/id_rsa_personal"
      ];
    };

    # Enable LiteLLM proxy for AI models
    litellm-proxy = {
      enable = true;
      models = [
        {
          name = config.claude-options.models.sonnet.id;
          model = config.claude-options.models.sonnet.name;
          aws_profile_name = config.claude-options.bedrock.profile;
        }
      ];
    };

    # Enable Open WebUI service
    open-webui.enable = true;
  };

  sops = {
    defaultSopsFile = ../../secrets/main.yaml;
    gnupg.home = profile.homeDirectory + "/.gnupg";
    secrets = {
      github_ssh_private_key_personal = {};
      github_token_bitgo = {};
      github_token_personal = {};
      jira_token_bitgo = {};
      youtube_api_key = {};
    };
  };
}
