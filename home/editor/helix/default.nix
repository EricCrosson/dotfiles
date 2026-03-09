{
  pkgs,
  inputs,
  ...
}: let
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
        language-servers = [];
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

  services.helix-theme-sync = {
    enable = true;
    settings = helixSettingsBase;
    light-theme = "wolf-alabaster-light-bg";
    dark-theme = "ao";
  };
}
