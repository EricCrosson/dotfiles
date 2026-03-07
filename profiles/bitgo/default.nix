{
  pkgs,
  profile,
  config,
  inputs,
  ...
}: let
  md2adf = pkgs.callPackage ../../pkgs/md2adf {};
  opPlugins = pkgs.callPackage ../../pkgs/op-plugins/plugins.nix {inherit inputs;};

  mcpServers = {
    chrome-devtools = {
      command = "${pkgs.lib.getExe' pkgs.nodejs "npx"}";
      args = ["-y" "chrome-devtools-mcp@latest" "--isolated"];
    };
    context7 = {
      command = "${pkgs.context7-mcp}/bin/context7-mcp";
    };
    figma = {
      type = "http";
      url = "https://mcp.figma.com/mcp";
    };
    matryoshka-rlm = {
      command = "${pkgs.lib.getExe' pkgs.nodejs "npx"}";
      args = ["-y" "-p" "matryoshka-rlm" "rlm-mcp"];
    };
  };

  mcpConfigFile = (pkgs.formats.json {}).generate "claude-mcp-servers.json" config.programs.claude-code.mcpServers;

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
    runtimeInputs = [pkgs.jq pkgs.alejandra pkgs.nodePackages.prettier];
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

  home = {
    file = {
    };

    packages = with pkgs;
      [
        amazon-ecr-credential-helper
        awscli2
        github-copilot-cli
      ]
      # git-disjoint/git-dl: use opPlugins system (no HM modules)
      ++ [opPlugins.git-disjoint.plugin opPlugins.git-disjoint.unwrapped opPlugins.git-disjoint.wrapper]
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

        md2adf
      ];

    activation = {
      installOpPlugins =
        config.lib.dag.entryAfter ["writeBoundary"]
        (opPlugins.mkActivationScript {
          pluginList = opPlugins.allPlugins.plugins;
        });

      # Sync MCP servers into ~/.claude.json (user scope) so Conductor picks
      # them up — its bundled claude binary doesn't use the Nix wrapper that
      # injects --mcp-config.
      syncClaudeMcpServers = config.lib.dag.entryAfter ["writeBoundary"] ''
        if [ -f "$HOME/.claude.json" ]; then
          cp "$HOME/.claude.json" "$HOME/.claude.json.bak"
          if ${pkgs.jq}/bin/jq --slurpfile servers ${mcpConfigFile} '.mcpServers = $servers[0]' \
            "$HOME/.claude.json.bak" > "$HOME/.claude.json.tmp" \
            && ${pkgs.jq}/bin/jq empty "$HOME/.claude.json.tmp" 2>/dev/null; then
            mv "$HOME/.claude.json.tmp" "$HOME/.claude.json"
          else
            echo "WARNING: failed to sync MCP servers into ~/.claude.json, restoring backup" >&2
            mv "$HOME/.claude.json.bak" "$HOME/.claude.json"
            rm -f "$HOME/.claude.json.tmp"
          fi
        fi
      '';
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
      zshIntegration = "deferred";
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
        bedrockOpusFile = config.sops.secrets.bedrock_opus_arn.path;
        bedrockSonnetFile = config.sops.secrets.bedrock_sonnet_arn.path;
        bedrockHaikuFile = config.sops.secrets.bedrock_haiku_arn.path;
      };
      inherit mcpServers;
      settings = {
        cleanupPeriodDays = 99999;
        skipDangerousModePermissionPrompt = true;
        env = {
          CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS = "1";
          DISABLE_TELEMETRY = "1";
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
        skillsDir = ../../claude/skills;
        teammateMode = "split-panes";
        permissions = {
          defaultMode = "plan";
        };
        theme = "dark";
      };
      skillsDir = ../../claude/skills;
      rulesDir = ../../claude/rules;
      agents.acli = ../../claude/agents/acli.md;
      memory.text = inputs.cortex.lib.cortex-instructions;
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
          condition = "gitdir:~/workspace/BitGo/";
          contents = workConfig;
        }
        {
          condition = "gitdir:~/workspace/ericcrosson-bitgo/";
          contents = workConfig;
        }
        {
          condition = "gitdir:~/.password-store/";
          contents = workConfig;
        }
      ];
      settings = {
        url = {
          "ssh://git@github.com-bitgo/BitGo/".insteadOf = "ssh://git@github.com/BitGo/";
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
          modelFile = config.sops.secrets.bedrock_sonnet_arn.path;
          aws_profile_name = config.claude-options.bedrock.profile;
        }
      ];
    };
  };

  sops = {
    defaultSopsFile = ../../secrets/main.yaml;
    gnupg.home = profile.homeDirectory + "/.gnupg";
    secrets = {
      aws_config = {
        path = "${config.home.homeDirectory}/.aws/config";
        mode = "0600";
      };
      bedrock_opus_arn = {};
      bedrock_sonnet_arn = {};
      bedrock_haiku_arn = {};
    };
  };
}
