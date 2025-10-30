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
      # mkvtoolnix-cli  # temporarily broken
      nodejs # Install npm
      obsidian
      openai-whisper
      poppler-utils # Install pdftotext for aichat
      yq-go

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

      # TODO: turn these into agents
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
      CLAUDE_CODE_GITHUB_TOKEN = "$(cat ${config.sops.secrets.claude_code_github_token.path} 2>/dev/null || echo '')";
      CLAUDE_CODE_ATLASSIAN_API_TOKEN = "$(cat ${config.sops.secrets.claude_code_atlassian_api_token.path} 2>/dev/null || echo '')";
      GITHUB_TOKEN = "$(cat ${config.sops.secrets.github_token_bitgo.path} 2>/dev/null || echo '')";
      GITHUB_TOKEN_BITGO_NIX = "$(cat ${config.sops.secrets.github_token_bitgo_nix.path} 2>/dev/null || echo '')";
      GITHUB_TOKEN_PERSONAL = "$(cat  ${config.sops.secrets.github_token_personal.path} 2>/dev/null || echo '')";
      GOOGLE_SERVICE_ACCOUNT_PRIVATE_KEY = "$(cat ${config.sops.secrets.google_service_account_private_key.path} 2>/dev/null || echo '')";
      JIRA_API_TOKEN = "$(cat ${config.sops.secrets.jira_token_bitgo.path} 2>/dev/null || echo '')";
      JIRA_USERNAME = "ericcrosson@bitgo.com";
      YOUTUBE_API_KEY = "$(cat ${config.sops.secrets.youtube_api_key.path} 2>/dev/null || echo '')";
      NIX_CONFIG = "access-tokens = github.com=\${GITHUB_TOKEN_BITGO_NIX}";
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
      package = pkgs.symlinkJoin {
        name = "claude-code-wrapped";
        paths = [pkgs.claude-code];
        buildInputs = [pkgs.makeWrapper];
        postBuild = ''
          wrapProgram $out/bin/claude \
            --set AWS_PROFILE "${config.aws-options.profile.default}" \
            --set AWS_REGION "${config.aws-options.region.default}"
        '';
      };
      commands = {
        grug = ''
          You are grug. You talk like grug. You code like grug. You do what grug do.
          grug help human, but grug warn human when grug know better.
        '';
      };
      mcpServers = {
        github = {
          type = "http";
          url = "https://api.githubcopilot.com/mcp/";
          headers = {
            Authorization = "Bearer \${CLAUDE_CODE_GITHUB_TOKEN}";
          };
        };
        jira = {
          command = "uvx";
          args = [
            "mcp-atlassian"
          ];
          env = {
            JIRA_URL = "https=//bitgoinc.atlassian.net";
            JIRA_USERNAME = "ericcrosson@bitgo.com";
            JIRA_API_TOKEN = "\${CLAUDE_CODE_ATLASSIAN_API_TOKEN}";
          };
        };
        confluence = {
          command = "uvx";
          args = [
            "mcp-atlassian"
          ];
          env = {
            CONFLUENCE_URL = "https=//bitgoinc.atlassian.net/wiki";
            CONFLUENCE_USERNAME = "ericcrosson@bitgo.com";
            CONFLUENCE_API_TOKEN = "\${CLAUDE_CODE_ATLASSIAN_API_TOKEN}";
          };
        };
        context7 = {
          command = "npx";
          args = [
            "-y"
            "@upstash/context7-mcp"
          ];
        };
      };
      settings = {
        cleanupPeriodDays = 99999;
        env = {
          CLAUDE_CODE_USE_BEDROCK = "1";
          DISABLE_TELEMETRY = "1";
        };
        model = "arn:aws:bedrock:us-west-2:319156457634:inference-profile/us.anthropic.claude-sonnet-4-5-20250929-v1:0";
        preferredNotifChannel = "terminal_bell";
        theme = "dark";
      };
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
      includes = let
        workConfig = {
          commit = {
            gpgSign = true;
          };
          core = {
            sshCommand = "ssh -i ~/.ssh/id_rsa";
          };
          tag = {
            gpgSign = true;
          };
          user = {
            email = "${profile.email}";
          };
          signing = {
            signingKey = "5BD755D7FD4AFCB6";
          };
        };
      in [
        {
          condition = "gitdir:~/workspace/BitGo/";
          contents = workConfig;
        }
        {
          condition = "gitdir:~/.password-store/";
          contents = workConfig;
        }
      ];
    };

    zsh = {
      shellAliases = {
        chat = "aichat";
        cmd = "aichat -e";
        grug = "claude '/grug'";
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
    # open-webui.enable = true;
  };

  sops = {
    defaultSopsFile = ../../secrets/main.yaml;
    gnupg.home = profile.homeDirectory + "/.gnupg";
    secrets = {
      claude_code_github_token = {};
      claude_code_atlassian_api_token = {};
      github_ssh_private_key_personal = {};
      github_token_bitgo = {};
      github_token_bitgo_nix = {};
      github_token_personal = {};
      google_service_account_private_key = {};
      jira_token_bitgo = {};
      youtube_api_key = {};
    };
  };
}
