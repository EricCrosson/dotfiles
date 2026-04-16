{
  pkgs,
  profile,
  config,
  inputs,
  ...
}: let
  opPlugins = pkgs.callPackage ../../pkgs/op-plugins/plugins.nix {inherit inputs;};
  chrome-devtools-mcp = pkgs.callPackage ../../pkgs/chrome-devtools-mcp {};

  claudePlugins = {
    context-mode = pkgs.callPackage ../../pkgs/context-mode-plugin {};
  };

  mcpServers = {
    chrome-devtools = {
      command = "${chrome-devtools-mcp}/bin/chrome-devtools-mcp";
      args = ["--isolated"];
    };
    context7 = {
      command = "${pkgs.context7-mcp}/bin/context7-mcp";
    };
  };

  claudeNotificationIcon = ../../claude/assets/claude-icon.png;

  claudeNotificationScript = pkgs.writeShellApplication {
    name = "claude-notification";
    runtimeInputs = [pkgs.jq pkgs.terminal-notifier];
    text =
      ''
        export CLAUDE_NOTIFICATION_ICON=${claudeNotificationIcon}
      ''
      + builtins.readFile ../../claude/hooks/notification.sh;
  };

  claudeFormatOnEditScript = pkgs.writeShellApplication {
    name = "claude-format-on-edit";
    runtimeInputs = [pkgs.jq pkgs.alejandra pkgs.prettier];
    text = builtins.readFile ../../claude/hooks/format-on-edit.sh;
  };

  # Force aws-saml to open Keycloak login in Safari instead of the default browser.
  # aws-saml uses pkg/browser which hardcodes `open <url>` on Darwin, ignoring $BROWSER.
  # We shadow `open` with a shim that routes through Safari.
  openInSafari = pkgs.writeShellScriptBin "open" ''
    exec /usr/bin/open -a Safari "$@"
  '';

  awsSaml = pkgs.writeShellScriptBin "aws-saml" ''
    export PATH=${openInSafari}/bin:$PATH
    exec ${inputs.aws-saml-bitgo.packages.${pkgs.system}.default}/bin/aws-saml "$@"
  '';
in {
  imports = [
    ./modules
    ../../modules/home-manager
    inputs._1password-shell-plugins.hmModules.default
    inputs.atlas.homeManagerModules.default
    inputs.cortex.homeManagerModules.default
  ];

  bitgo.ssh.enable = true;
  bitgo.sops.enable = true;

  home = {
    file = {
    };

    packages = with pkgs;
      [
        amazon-ecr-credential-helper
        awscli2
        github-copilot-cli
      ]
      ++ [inputs.git-disjoint.packages.${pkgs.system}.default]
      # git-dl: use opPlugins system (no HM modules)
      ++ [opPlugins.git-dl.plugin opPlugins.git-dl.unwrapped opPlugins.git-dl.wrapper]
      ++ [
        gh
        k9s
        kubectl
        kubectx
        kustomize
        nodejs # Install npm
        openai-whisper
        poppler-utils # Install pdftotext for aichat
        yq-go

        inputs.aws-console-bitgo.packages.${pkgs.system}.default
        awsSaml

        # 1Password CLI for secret management
        _1password-cli
      ];

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
      plugins = [];
    };

    aichat = {
      enable = true;
      settings = {
        model = "bedrock-claude:${config.claude-options.models.default}";
        stream = true;
        save = true;
        keybindings = "emacs";
        wrap = "auto";
        save_shell_history = true;
        clients = [
          {
            type = "openai-compatible";
            name = "bedrock-claude";
            api_base = config.services.litellm-proxy.apiUrl;
            api_key = "xxx";
            models = [
              {
                name = config.claude-options.models.sonnet.id;
                max_input_tokens = config.claude-options.models.sonnet.contextLength;
                supports_function_calling = false;
                supports_vision = false;
              }
            ];
          }
        ];
      };
    };

    atlas = {
      enable = true;
      package = let
        atlas = inputs.atlas.packages.${pkgs.system}.default;
      in
        pkgs.writeShellApplication {
          name = "atlas";
          runtimeInputs = [atlas];
          text = ''
            if [[ -r ${config.bitgo.sops.secretPaths.atlas_bedrock_model_id} ]]; then
              export ATLAS_BEDROCK_MODEL_ID
              ATLAS_BEDROCK_MODEL_ID="$(< ${config.bitgo.sops.secretPaths.atlas_bedrock_model_id})"
            fi
            exec atlas "$@"
          '';
        };
      zshIntegration = "deferred";
      settings = {
        title = {
          mode = "bedrock";
          bedrock.profile = config.claude-options.bedrock.profile;
        };
      };
    };

    claude.theme-sync.enable = true;

    conductor.enable = false;

    codex = {
      enable = true;
      skills = ../../claude/skills;
      settings = {
        mcp_servers = mcpServers;
      };
    };

    cortex = {
      enable = true;
      url = "http://127.0.0.1:3001";
    };

    claude-code = {
      enable = true;
      package = pkgs.callPackage ../../pkgs/claude-wrapper {} {
        inherit (pkgs) claude-code;
        bedrockProfile = config.claude-options.bedrock.profile;
        bedrockRegion = config.claude-options.bedrock.region;
        bedrockOpusFile = config.bitgo.sops.secretPaths.bedrock_opus_arn;
        bedrockSonnetFile = config.bitgo.sops.secretPaths.bedrock_sonnet_arn;
        bedrockHaikuFile = config.bitgo.sops.secretPaths.bedrock_haiku_arn;
        pluginDirs = [claudePlugins.context-mode];
      };
      inherit mcpServers;
      context = inputs.cortex.lib.cortex-instructions;
      skills = ../../claude/skills;
      rulesDir = ../../claude/rules;
      settings = {
        cleanupPeriodDays = 99999;
        skipDangerousModePermissionPrompt = true;
        terminalProgressBarEnabled = false;
        env = {
          CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS = "1";
          ENABLE_TOOL_SEARCH = "1";
        };
        hooks = {
          Notification = [
            {
              hooks = [
                {
                  type = "command";
                  command = "${pkgs.lib.getExe claudeNotificationScript}";
                }
              ];
            }
          ];
          PostToolUse = [
            {
              matcher = "Edit|Write";
              hooks = [
                {
                  type = "command";
                  command = "${pkgs.lib.getExe claudeFormatOnEditScript}";
                }
              ];
            }
          ];
        };
        teammateMode = "split-panes";
        permissions = {
          defaultMode = "plan";
        };
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
            "ssh://git@github.com-bitgo/ericcrosson-bitgo/".insteadOf = "https://github.com/ericcrosson-bitgo/";
            "git@github.com-bitgo:ericcrosson-bitgo/".insteadOf = "git@github.com:ericcrosson-bitgo/";
          };
          user = {
            email = "${profile.email}";
            signingKey = "5BD755D7FD4AFCB6";
          };
        };
      in [
        {
          condition = "hasconfig:remote.*.url:*github.com*BitGo/**";
          contents = workConfig;
        }
        {
          condition = "hasconfig:remote.*.url:*github.com/bitgo/**";
          contents = workConfig;
        }
        {
          condition = "hasconfig:remote.*.url:*github.com*ericcrosson-bitgo/**";
          contents = workConfig;
        }
      ];
      settings = {
        url = {
          # Global rewrites — apply unconditionally at clone time (before any
          # includeIf evaluates), so `gh repo clone` from temp dirs works.
          # Covers both HTTPS and SSH forms, and both BitGo/ and bitgo/ cases.
          "ssh://git@github.com-bitgo/BitGo/" = {
            insteadOf = [
              "https://github.com/BitGo/"
              "https://github.com/bitgo/"
              "ssh://git@github.com/BitGo/"
            ];
          };
          "git@github.com-bitgo:BitGo/" = {
            insteadOf = [
              "git@github.com:BitGo/"
              "git@github.com:bitgo/"
            ];
          };
          "ssh://git@github.com-bitgo/ericcrosson-bitgo/" = {
            insteadOf = ["https://github.com/ericcrosson-bitgo/"];
          };
          "git@github.com-bitgo:ericcrosson-bitgo/" = {
            insteadOf = ["git@github.com:ericcrosson-bitgo/"];
          };
        };
      };
    };

    zsh = {
      initContent = ''
        # Background gpg-agent tty update (doesn't need to block startup)
        export GPG_TTY=$TTY
        ${pkgs.gnupg}/bin/gpg-connect-agent --quiet updatestartuptty /bye > /dev/null &!
      '';
      shellAliases = {
        chat = "aichat";
        cmd = "aichat -e";
      };
    };
  };

  services = {
    litellm-proxy = {
      enable = true;
      aws-saml = awsSaml;
      models = [
        {
          name = config.claude-options.models.sonnet.id;
          modelFile = config.bitgo.sops.secretPaths.bedrock_sonnet_arn;
          aws_profile_name = config.claude-options.bedrock.profile;
        }
      ];
    };
  };
}
