{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.programs.omp;
  yamlFormat = pkgs.formats.yaml {};
  jsonFormat = pkgs.formats.json {};
  baseConfig = yamlFormat.generate "omp-base-config.yml" cfg.settings;
  mcpBaseConfig = jsonFormat.generate "omp-mcp-base.json" {
    "$schema" = "https://raw.githubusercontent.com/can1357/oh-my-pi/main/packages/coding-agent/src/config/mcp-schema.json";
    inherit (cfg) mcpServers;
  };
  ompConfigSync = pkgs.callPackage ../../../../pkgs/omp-config-sync {};
  ompMcpSync = pkgs.callPackage ../../../../pkgs/omp-mcp-sync {};
  ompCompletions = pkgs.callPackage ../../../../pkgs/omp-completions {omp = cfg.package;};
in {
  options.programs.omp = {
    enable = mkEnableOption "Oh My Pi (omp) configuration";
    package = mkOption {
      type = types.package;
      description = ''
        The omp package to install and to derive shell completions from.
        Hosts must set this to the flake-input package so completions always
        match the installed binary version.
      '';
    };

    settings = mkOption {
      inherit (yamlFormat) type;
      default = {};
      description = ''
        Configuration written to ~/.omp/agent/config.yml.
      '';
      example = literalExpression ''
        {
          advisor.enabled = true;
          modelProviderOrder = [ "openrouter" ];
        }
      '';
    };

    models = mkOption {
      inherit (yamlFormat) type;
      default = {};
      description = ''
        Configuration written to ~/.omp/agent/models.yml.
      '';
      example = literalExpression ''
        {
          providers.openrouter.apiKey = "!cat /path/to/key";
        }
      '';
    };

    env = mkOption {
      type = types.attrsOf types.str;
      default = {};
      description = ''
        Environment variables written to ~/.omp/agent/.env.
      '';
      example = literalExpression ''
        {
          GOOGLE_CLOUD_PROJECT = "ai-enablement-500217";
          GOOGLE_CLOUD_LOCATION = "global";
          SMART_CD_LS = "false";
          SMART_CD_GIT_STATUS = "false";
        }
      '';
    };

    mcpServers = mkOption {
      inherit (jsonFormat) type;
      default = {};
      description = ''
        MCP servers written to ~/.omp/agent/mcp.json. Authoritative on every
        activation: servers absent from this option are removed from the file;
        only omp's runtime state (disabledServers/enabledServers) is preserved.
      '';
    };

    formatMarkdown = mkOption {
      type = types.submodule {
        options = {
          enable = mkEnableOption "prose-wrap markdown after a write";
          prettier = mkOption {
            type = types.package;
            default = pkgs.prettier;
            description = "Prettier binary used to format markdown.";
          };
        };
      };
      default = {};
      description = ''
        Render an omp extension that runs prettier with --prose-wrap=always on
        markdown files after the write tool completes. Deterministic: the tool
        result is delivered to the model only after formatting finishes.
      '';
    };
  };

  config = mkIf cfg.enable {
    home = {
      activation = {
        syncOmpConfig = config.lib.dag.entryAfter ["linkGeneration"] ''
          if [[ -v DRY_RUN ]]; then
            echo "Would synchronize writable Oh My Pi configuration"
          else
            ${ompConfigSync}/bin/omp-config-sync \
              ${baseConfig} \
              "$HOME/.omp/agent/config.yml"
          fi
        '';

        # Merge-syncs the generated base into ~/.omp/agent/mcp.json (base is
        # authoritative; omp's disabledServers/enabledServers write-backs are
        # preserved). Hosts that enable programs.omp without setting
        # mcpServers sync an empty server set.
        syncOmpMcpConfig = config.lib.dag.entryAfter ["linkGeneration"] ''
          if [[ -v DRY_RUN ]]; then
            echo "Would synchronize writable Oh My Pi MCP configuration"
          else
            ${ompMcpSync}/bin/omp-mcp-sync \
              ${mcpBaseConfig} \
              "$HOME/.omp/agent/mcp.json"
          fi
        '';
        # compinit trusts its cached completion map (compinit -C) and only
        # rebuilds weekly, so it never notices new fpath entries. Invalidate
        # the dump once per activation so _omp registers on the next shell.
        removeStaleOmpCompdump = config.lib.dag.entryAfter ["linkGeneration"] ''
          rm -f "$HOME/.zcompdump" "$HOME/.zcompdump.zwc"
        '';
      };

      file = {
        ".omp/agent/models.yml" = mkIf (cfg.models != {}) {
          source = yamlFormat.generate "omp-models.yml" cfg.models;
        };

        ".omp/agent/.env" = mkIf (cfg.env != {}) {
          text = concatStringsSep "\n" (mapAttrsToList (k: v: "${k}=${v}") cfg.env) + "\n";
        };

        ".omp/agent/extensions/format-md.ts" = mkIf cfg.formatMarkdown.enable {
          text = ''
            import type { ExtensionAPI } from "@oh-my-pi/pi-coding-agent";

            export default function formatProse(pi: ExtensionAPI): void {
              pi.on("tool_result", async (event) => {
                if (event.isError) return;
                if (event.toolName !== "write") return;

                const path = event.input.path;
                if (typeof path !== "string" || !/\.mdx?$/.test(path)) return;

                const res = await pi.exec(
                  "${cfg.formatMarkdown.prettier}/bin/prettier",
                  ["--prose-wrap=always", "--write", "--log-level=warn", path],
                );
                if (res.code !== 0) {
                  pi.logger.warn(`prettier failed for ''${path}: ''${res.stderr}`);
                }
              });
            }
          '';
        };
      };

      packages =
        [cfg.package ompCompletions]
        ++ optionals cfg.formatMarkdown.enable [
          cfg.formatMarkdown.prettier
        ];
    };

    # Completion scripts are pre-generated at build time and served from
    # fpath; compinit (deferred via zsh-defer) links _omp from the cached
    # dump — nothing executes at shell startup.
    programs.zsh.initContent = ''
      fpath+=(${ompCompletions}/share/zsh/site-functions)
    '';
  };
}
