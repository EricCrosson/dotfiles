{
  pkgs,
  profile,
  config,
  inputs,
  ...
}: let
  opPlugins = pkgs.callPackage ../../pkgs/op-plugins/plugins.nix {inherit inputs;};
in {
  imports = [
    ./modules
    ../../modules/home-manager
    inputs._1password-shell-plugins.hmModules.default
  ];

  bitgo.ssh.enable = true;

  home = {
    file = {
      ".config/.jira/.config.yml" = {
        text = ''
          server: https://bitgoinc.atlassian.net
          login: ${profile.email}
          installation: cloud
          project:
            key: DX
          board:
            name: "Default board"
            type: scrum
          issue:
            types:
              - id: "10101"
                name: Task
                handle: Task
                subtask: false
              - id: "10100"
                name: Story
                handle: Story
                subtask: false
              - id: "10103"
                name: Bug
                handle: Bug
                subtask: false
              - id: "10447"
                name: Spike
                handle: Spike
                subtask: false
        '';
      };
    };

    packages = with pkgs;
      [
        amazon-ecr-credential-helper
        awscli2
        github-copilot-cli
      ]
      # Claude: plugin + unwrapped from opPlugins (wrapper via HM module)
      ++ [opPlugins.claude.plugin opPlugins.claude.unwrapped]
      # Jira/git-disjoint: use opPlugins system (no HM modules)
      ++ [opPlugins.jira.plugin opPlugins.jira.unwrapped opPlugins.jira.wrapper]
      ++ [opPlugins.git-disjoint.plugin opPlugins.git-disjoint.unwrapped opPlugins.git-disjoint.wrapper]
      ++ [
        jiratui
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
      ];

    sessionVariables = {
      ZED_AWS_PROFILE = "dev";
    };

    activation = {
      installOpPlugins =
        config.lib.dag.entryAfter ["writeBoundary"]
        (opPlugins.mkActivationScript {
          pluginList = opPlugins.allPlugins.plugins;
        });
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

    claude-code = {
      enable = true;
      # Wrapper inlined: routes through 1Password (passes command name, not path)
      package = pkgs.writeShellScriptBin "claude" ''
        exec ${pkgs._1password-cli}/bin/op plugin run -- claude-unwrapped "$@"
      '';
      mcpServers = {
        github = {
          type = "http";
          url = "https://api.githubcopilot.com/mcp/";
          headers = {
            Authorization = "Bearer \${CLAUDE_CODE_GITHUB_TOKEN}";
          };
        };
        context7 = {
          command = "${pkgs.context7-mcp}/bin/context7-mcp";
        };
        chrome-devtools = {
          command = "${pkgs.lib.getExe' pkgs.nodejs "npx"}";
          args = ["-y" "chrome-devtools-mcp@latest"];
        };
      };
      settings = {
        cleanupPeriodDays = 99999;
        env = {
          CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS = "1";
          DISABLE_TELEMETRY = "1";
          ENABLE_TOOL_SEARCH = "1";
        };
        model = "sonnet";
        skillsDir = ../../claude/skills;
        teammateMode = "split-panes";
        preferredNotifChannel = "terminal_bell";
        theme = "dark";
      };
      skillsDir = ../../claude/skills;
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
