{
  pkgs,
  lib,
  profile,
  config,
  inputs,
  ...
}: let
  chrome-devtools-mcp = pkgs.callPackage ../../pkgs/chrome-devtools-mcp {};
  mcp-remote = pkgs.callPackage ../../pkgs/mcp-remote {};
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
        command = "${mcp-remote}/bin/mcp-remote";
        args = ["https://mcp.linear.app/mcp"];
      };
    };
  codexSettings = {
    model_provider = "openrouter";
    model = "openai/gpt-5.6-luna";
    model_reasoning_effort = "medium";
    mcp_servers = mcpServers;
    model_providers.openrouter = {
      name = "openrouter";
      base_url = "https://openrouter.ai/api/v1";
      auth = {
        command = "sh";
        args = ["-c" "echo $OPENROUTER_API_KEY"];
      };
    };
  };
  codexConfig = (pkgs.formats.toml {}).generate "codex-config" codexSettings;
  codexConfigSync = pkgs.callPackage ../../pkgs/codex-config-sync {};
  codex = pkgs.writeShellApplication {
    name = "codex";
    runtimeInputs = [pkgs.codex];
    text = ''
      if [[ -r ${config.bitgo.sops.secretPaths.openrouter_api_key} ]]; then
        export OPENROUTER_API_KEY
        OPENROUTER_API_KEY="$(< ${config.bitgo.sops.secretPaths.openrouter_api_key})"
      fi
      exec ${pkgs.codex}/bin/codex "$@"
    '';
  };

  rulesDir = ../../ai/rules;
  rulesContext = lib.concatStringsSep "\n\n" (
    map (name: builtins.readFile (rulesDir + "/${name}"))
    (builtins.attrNames (lib.filterAttrs (n: _: lib.hasSuffix ".md" n)
        (builtins.readDir rulesDir)))
  );
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
    inputs._1password-shell-plugins.hmModules.default
  ];

  bitgo.ssh.enable = true;
  programs.gh.extensions = [
    inputs.gh-endorse.packages.${pkgs.system}.gh-endorse
    inputs.gh-gantt.packages.${pkgs.system}.gh-gantt
  ];

  home = {
    file = {
      ".gemini/antigravity-cli/settings.json".force = true;
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
      poppler-utils # Install pdftotext for aichat
      yq-go

      inputs.aws-console-bitgo.packages.${pkgs.system}.default
      awsSaml

      # 1Password CLI for secret management
      _1password-cli
    ];

    activation = {
      syncCodexConfig = config.lib.dag.entryAfter ["linkGeneration"] ''
        if [[ -v DRY_RUN ]]; then
          echo "Would synchronize writable Codex configuration"
        else
          ${codexConfigSync}/bin/codex-config-sync \
            ${codexConfig} \
            "$HOME/.codex/config.toml"
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
            api_base = "http://127.0.0.1:4000/v1";
            models = [
              {
                name = config.claude-options.models.sonnet.id;
                max_input_tokens = config.claude-options.models.sonnet.contextLength;
                supports_function_calling = false;
                supports_vision = true;
              }
              {
                name = config.claude-options.models.haiku.id;
                max_input_tokens = config.claude-options.models.haiku.contextLength;
                supports_function_calling = false;
                supports_vision = true;
              }
              {
                name = config.claude-options.models.opus.id;
                max_input_tokens = config.claude-options.models.opus.contextLength;
                supports_function_calling = false;
                supports_vision = true;
              }
              {
                name = "gemini-3.5-flash";
                max_input_tokens = 2000000;
                supports_function_calling = true;
                supports_vision = true;
              }
              {
                name = "gemini-3.5-flash-lite";
                max_input_tokens = 1000000;
                supports_function_calling = true;
                supports_vision = true;
              }
              {
                name = "gemini-3.8-flash";
                max_input_tokens = 2000000;
                supports_function_calling = true;
                supports_vision = true;
              }
            ];
          }
        ];
      };
    };

    antigravity-cli = {
      enable = true;
      skills = ../../ai/skills;
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
      package = codex;
      skills = ../../ai/skills;
      # Codex persists project trust in config.toml, so it cannot be a Nix store symlink.
      settings = null;
    };
    opencode = {
      enable = true;
      package = pkgs.writeShellApplication {
        name = "opencode";
        runtimeInputs = [pkgs.opencode];
        text = ''
          if [[ -r ${config.bitgo.sops.secretPaths.openrouter_api_key} ]]; then
            export OPENROUTER_API_KEY
            OPENROUTER_API_KEY="$(< ${config.bitgo.sops.secretPaths.openrouter_api_key})"
          fi
          exec ${pkgs.opencode}/bin/opencode "$@"
        '';
      };
      enableMcpIntegration = true;
      context = rulesContext;
      settings = {
        model = "openrouter/openrouter/auto";
        small_model = "openrouter/anthropic/claude-3.5-haiku";
        provider = {
          openrouter = {
            models = {
              "openrouter/auto" = {};
              "anthropic/claude-3.7-sonnet" = {};
              "anthropic/claude-3.5-haiku" = {};
            };
          };
        };
        mcp =
          lib.mapAttrs (_: server: {
            type = "local";
            command = [server.command] ++ (server.args or []);
            enabled = true;
          })
          mcpServers;
      };
      tui = {
        theme = "system";
        keybinds = {
          leader = "alt+b";
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

        # aichat authenticates to the local litellm proxy via its master key.
        export BEDROCK_CLAUDE_API_KEY="$(cat ${config.bitgo.sops.secretPaths.litellm_master_key})"
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
      host = "0.0.0.0";
      masterKeyFile = config.bitgo.sops.secretPaths.litellm_master_key;
      aws-saml = awsSaml;
      models = [
        {
          name = config.claude-options.models.sonnet.id;
          modelFile = config.bitgo.sops.secretPaths.bedrock_sonnet_arn;
          aws_profile_name = config.claude-options.bedrock.profile;
        }
        {
          name = config.claude-options.models.haiku.id;
          modelFile = config.bitgo.sops.secretPaths.bedrock_haiku_arn;
          aws_profile_name = config.claude-options.bedrock.profile;
        }
        {
          name = config.claude-options.models.opus.id;
          modelFile = config.bitgo.sops.secretPaths.bedrock_opus_arn;
          aws_profile_name = config.claude-options.bedrock.profile;
        }
        {
          name = "gemini-3.5-flash";
          model = "vertex_ai/gemini-3.5-flash";
          extraConfig = ''
            {
              "vertex_project": "ai-enablement-500217",
              "vertex_location": "us"
            }
          '';
        }
        {
          name = "gemini-3.5-flash-lite";
          model = "vertex_ai/gemini-3.5-flash-lite";
          extraConfig = ''
            {
              "vertex_project": "ai-enablement-500217",
              "vertex_location": "us"
            }
          '';
        }
        {
          name = "gemini-3.8-flash";
          model = "vertex_ai/gemini-3.8-flash";
          extraConfig = ''
            {
              "vertex_project": "ai-enablement-500217",
              "vertex_location": "us"
            }
          '';
        }
      ];
    };
  };
}
