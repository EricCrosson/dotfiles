{
  config,
  lib,
  pkgs,
  inputs,
  ...
}: let
  tomlFormat = pkgs.formats.toml {};

  helixSettingsBase = {
    keys.normal = {
      C-h = "jump_view_left";
      C-j = "jump_view_down";
      C-k = "jump_view_up";
      C-l = "jump_view_right";
      space = {
        l = ":reload";
        L = ":reload-all";
        o = ":reflow";
        t = ":toggle-option lsp.display-inlay-hints";
        T = ":tree-sitter-subtree";
      };
    };
    editor = {
      idle-timeout = 0;
      cursor-shape = {
        normal = "block";
        insert = "bar";
        select = "underline";
      };
      file-picker = {
        hidden = false;
      };
      lsp = {
        display-inlay-hints = false;
      };
      inline-diagnostics = {
        cursor-line = "error";
        other-lines = "error";
      };
    };
  };

  lightConfig =
    tomlFormat.generate "helix-config-light"
    (helixSettingsBase // {theme = "catppuccin_latte";});

  darkConfig =
    tomlFormat.generate "helix-config-dark"
    (helixSettingsBase // {theme = "ao";});

  helixConfigDir = "${config.home.homeDirectory}/.config/helix";

  syncScript = pkgs.writeShellApplication {
    name = "helix-theme-sync";
    runtimeInputs = with pkgs; [coreutils];
    text = ''
      # Detect macOS appearance
      if defaults read -g AppleInterfaceStyle &>/dev/null; then
        target="${darkConfig}"
      else
        target="${lightConfig}"
      fi

      config_path="${helixConfigDir}/config.toml"

      # Check if symlink already points to the correct target
      current="$(readlink "$config_path" 2>/dev/null || true)"
      if [ "$current" = "$target" ]; then
        exit 0
      fi

      # Atomically swap the symlink
      ln -sf "$target" "$config_path.tmp"
      mv "$config_path.tmp" "$config_path"

      # Signal all running Helix instances to reload config
      pkill -USR1 hx || true
    '';
  };
in {
  home.packages = with pkgs; [
    dockerfile-language-server
    gopls
    hadolint
    jsonnet-language-server
    ltex-ls
    nil
    nodePackages.bash-language-server
    nodePackages.typescript-language-server
    nodePackages.vscode-langservers-extracted
    shellcheck
    taplo # TOML LSP

    # Python
    pyright
    ruff
  ];

  programs.helix = {
    enable = true;
    package = inputs.helix.packages.${pkgs.system}.default;
    languages.language = [
      {
        name = "markdown";
        language-servers = [
          {
            name = "ltex-ls";
          }
        ];
      }
      {
        name = "python";
        language-servers = [
          {
            name = "pyright";
          }
        ];
      }
    ];
  };

  launchd-with-logs.services.helix-theme-sync = {
    command = "${syncScript}/bin/helix-theme-sync";
    runAtLoad = true;
    watchPaths = ["${config.home.homeDirectory}/Library/Preferences/.GlobalPreferences.plist"];
    logging = {
      stderr = "${config.home.homeDirectory}/Library/Logs/helix-theme-sync.error.log";
    };
  };

  home.activation.syncHelixTheme = lib.hm.dag.entryAfter ["writeBoundary"] ''
    run ${syncScript}/bin/helix-theme-sync
  '';
}
