{
  pkgs,
  profile,
  config,
  inputs,
  ...
}: {
  imports = [
    ./modules
    ../../modules/home-manager
    inputs._1password-shell-plugins.hmModules.default
  ];

  bitgo.ssh.enable = true;

  home = {
    file = {
      # TODO: turn these into agents for improved context management.
      ".claude/CLAUDE.md" = {
        source = ../../.claude/CLAUDE.md;
      };
    };

    packages = with pkgs; [
      amazon-ecr-credential-helper
      awscli2
      k9s
      kubectl
      kubectx
      kustomize
      nodejs # Install npm
      openai-whisper
      poppler-utils # Install pdftotext for aichat
      yq-go

      inputs.percentage-changed-calculator.packages.${pkgs.system}.default

      inputs.aws-console-bitgo.packages.${pkgs.system}.default
      inputs.aws-saml-bitgo.packages.${pkgs.system}.default

      # 1Password CLI for secret management
      _1password-cli

      # Custom 1Password shell plugin for git-disjoint
      (pkgs.callPackage ../../pkgs/op-plugin-git-disjoint {})
    ];

    sessionVariables = {
      ZED_AWS_PROFILE = "dev";
    };

    activation = {
      installOpPlugin = config.lib.dag.entryAfter ["writeBoundary"] ''
        run mkdir -p ~/.op/plugins/local
        run chmod 700 ~/.op ~/.op/plugins ~/.op/plugins/local 2>/dev/null || true
        run cp -f ${pkgs.callPackage ../../pkgs/op-plugin-git-disjoint {}}/bin/op-plugin-git-disjoint \
          ~/.op/plugins/local/
      '';
    };
  };

  programs = {
    _1password-shell-plugins = {
      enable = true;
      plugins = with pkgs; [
        gh
      ];
    };

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
      package = let
        basePackage = pkgs.symlinkJoin {
          name = "claude-code-wrapped";
          paths = [pkgs.claude-code];
          buildInputs = [pkgs.makeWrapper];
          # Add ~/.local/bin for beads
          postBuild = ''
            wrapProgram $out/bin/claude \
              --prefix PATH : "$HOME/.local/bin" \
              --set AWS_PROFILE "${config.aws-options.profile.default}" \
              --set AWS_REGION "${config.aws-options.region.default}"
          '';
          meta.mainProgram = "claude";
        };
      in
        # TODO(opsec): migrate to proper 1Password plugins
        # Temporary: using direct op read until plugins created
        pkgs.symlinkJoin {
          name = "claude-code-with-secrets";
          paths = [basePackage];
          buildInputs = [pkgs.makeWrapper pkgs._1password-cli];
          postBuild = ''
            wrapProgram $out/bin/claude \
              --run 'export CLAUDE_CODE_GITHUB_TOKEN=$(op read "op://Nix-Secrets/claude-code-github-token/token" 2>/dev/null || true)' \
              --run 'export CLAUDE_CODE_ATLASSIAN_API_TOKEN=$(op read "op://Nix-Secrets/claude-code-atlassian-api-token/token" 2>/dev/null || true)'
          '';
          meta.mainProgram = "claude";
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
            JIRA_USERNAME = "${profile.email}";
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
            JIRA_USERNAME = "${profile.email}";
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

    git = {
      includes = let
        workConfig = {
          credential = {
            username = "ericcrosson-bitgo";
          };
          gpg = {
            format = "openpgp";
            program = "${pkgs.gnupg}/bin/gpg";
          };
          url = {
            # Rewrite BitGo URLs to use github.com-bitgo SSH alias
            # This ensures work repos use the optimized SSH config with ControlMaster
            "ssh://git@github.com-bitgo/BitGo/".insteadOf = "https://github.com/BitGo/";
            "git@github.com-bitgo:BitGo/".insteadOf = "git@github.com:BitGo/";
          };
          user = {
            email = "${profile.email}";
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

    llm = {
      enable = true;
      models = [
        {
          inherit (config.claude-options.models.sonnet) id name;
          api_base = config.services-options.litellm-proxy.baseUrl;
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
    litellm-proxy = {
      enable = true;
      aws-saml = inputs.aws-saml-bitgo.packages.${pkgs.system}.default;
      models = [
        {
          name = config.claude-options.models.sonnet.id;
          model = config.claude-options.models.sonnet.name;
          aws_profile_name = config.claude-options.bedrock.profile;
        }
      ];
    };

    # open-webui.enable = true;
  };

  sops = {
    defaultSopsFile = ../../secrets/main.yaml;
    gnupg.home = profile.homeDirectory + "/.gnupg";
    secrets = {
      github_ssh_private_key_personal = {};
    };
  };
}
