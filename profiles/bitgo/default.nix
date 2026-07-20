{
  pkgs,
  lib,
  profile,
  config,
  inputs,
  ...
}: let
  chrome-devtools-mcp = pkgs.callPackage ../../pkgs/chrome-devtools-mcp {};

  standaloneClaude = pkgs.runCommand "standalone-claude" {} ''
    mkdir -p $out/bin
    ln -s ${config.home.homeDirectory}/.local/bin/claude $out/bin/claude
  '';

  baseMcpServers = {
    chrome-devtools = {
      command = "${chrome-devtools-mcp}/bin/chrome-devtools-mcp";
      args = ["--isolated"];
    };
    context7 = {
      command = "${pkgs.context7-mcp}/bin/context7-mcp";
    };
  };

  mcpServers =
    baseMcpServers
    // {
      linear = {
        command = "npx";
        args = ["-y" "mcp-remote" "https://mcp.linear.app/mcp"];
      };
    };

  rulesDir = ../../claude/rules;
  rulesContext = lib.concatStringsSep "\n\n" (
    map (name: builtins.readFile (rulesDir + "/${name}"))
    (builtins.attrNames (lib.filterAttrs (n: _: lib.hasSuffix ".md" n)
        (builtins.readDir rulesDir)))
  );

  claudeNotificationScript = pkgs.writeShellApplication {
    name = "claude-notification";
    runtimeInputs = [pkgs.jq];
    text = builtins.readFile ../../claude/hooks/notification.sh;
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
  ];

  bitgo.ssh.enable = true;
  bitgo.sops.enable = true;

  home = {
    file = {
    };

    packages = with pkgs; [
      agent-browser
      amazon-ecr-credential-helper
      awscli2
      cloudflared
      gh
      github-copilot-cli
      google-cloud-sdk
      inputs.git-disjoint.packages.${pkgs.system}.default
      inputs.git-dl.packages.${pkgs.system}.default
      k9s
      kubectl
      kubectx
      kustomize
      nodejs # Install npm
      poppler-utils # Install pdftotext for aichat
      yq-go

      inputs.aws-console-bitgo.packages.${pkgs.system}.default
      awsSaml

      # 1Password CLI for secret management
      _1password-cli
    ];

    activation = {
      installClaude = config.lib.dag.entryAfter ["writeBoundary"] ''
        if [ ! -x "${config.home.homeDirectory}/.local/bin/claude" ]; then
          export PATH="/usr/bin:/bin:/usr/sbin:/sbin:$PATH"
          ${pkgs.curl}/bin/curl -fsSL https://claude.ai/install.sh | /bin/sh
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

    antigravity-cli = {
      enable = true;
      skills = ../../claude/skills;
      inherit mcpServers;
      context = {
        GEMINI = rulesContext;
      };
      settings = {
        colorScheme = "light";
        enableTelemetry = false;
        gcp = {
          project = "ai-enablement-500217";
          location = "us";
        };
        model = "Gemini 3.5 Flash (Medium)";
        runningLightSpeed = "fast";
        trustedWorkspaces = [
          "/Users/ericcrosson/workspace/BitGo"
          "/Users/ericcrosson/workspace/EricCrosson"
          "/Users/ericcrosson/workspace/ericcrosson-bitgo"
        ];
      };
      permissions = {
        allow = [
          "mcp(linear/get_issue)"
          "mcp(linear/list_issues)"
          "command(git config)"
          "mcp(linear/list_comments)"
          "command(which)"
          "mcp(linear/search_documentation)"
        ];
      };
    };

    codex = {
      enable = true;
      skills = ../../claude/skills;
      settings = {
        mcp_servers = mcpServers;
      };
    };

    claude-code = {
      enable = true;
      package =
        (pkgs.callPackage ../../pkgs/claude-wrapper {} {
          claude-code = standaloneClaude;
          bedrockProfile = config.claude-options.bedrock.profile;
          bedrockRegion = config.claude-options.bedrock.region;
          bedrockOpusFile = config.bitgo.sops.secretPaths.bedrock_opus_arn;
          bedrockSonnetFile = config.bitgo.sops.secretPaths.bedrock_sonnet_arn;
          bedrockHaikuFile = config.bitgo.sops.secretPaths.bedrock_haiku_arn;
        })
        // {version = "2.1.206";};
      mcpServers = baseMcpServers;
      skills = ../../claude/skills;
      inherit rulesDir;
      settings = {
        autoUpdates = true;
        cleanupPeriodDays = 99999;
        showClearContextOnPlanAccept = true;
        skipDangerousModePermissionPrompt = true;
        terminalProgressBarEnabled = false;
        env = {
          CLAUDE_CODE_EXPERIMENTAL_AGENT_TEAMS = "1";
          DISABLE_INSTALLATION_CHECKS = "1";
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
        teammateMode = "auto";
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
          condition = "hasconfig:remote.*.url:ssh://*github.com*/BitGo/**";
          contents = workConfig;
        }
        {
          condition = "hasconfig:remote.*.url:https://*github.com*/BitGo/**";
          contents = workConfig;
        }
        {
          condition = "hasconfig:remote.*.url:*github.com*bitgo/**";
          contents = workConfig;
        }
        {
          condition = "hasconfig:remote.*.url:ssh://*github.com*/bitgo/**";
          contents = workConfig;
        }
        {
          condition = "hasconfig:remote.*.url:https://*github.com*/bitgo/**";
          contents = workConfig;
        }
        {
          condition = "hasconfig:remote.*.url:*github.com*ericcrosson-bitgo/**";
          contents = workConfig;
        }
        {
          condition = "hasconfig:remote.*.url:ssh://*github.com*/ericcrosson-bitgo/**";
          contents = workConfig;
        }
        {
          condition = "hasconfig:remote.*.url:https://*github.com*/ericcrosson-bitgo/**";
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
        omp = lib.mkForce "OPENROUTER_API_KEY=$(cat ${config.bitgo.sops.secretPaths.openrouter_api_key}) GOOGLE_CLOUD_PROJECT=ai-enablement-500217 GOOGLE_CLOUD_LOCATION=global omp --provider openrouter";
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
