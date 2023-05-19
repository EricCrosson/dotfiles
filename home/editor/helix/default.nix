{
  pkgs,
  user,
  inputs,
  ...
}: {
  # DISCUSS: moving my helix config to its own flake for reuse

  home.packages = with pkgs; [
    inputs.jsonnet-language-server.packages.${pkgs.system}.jsonnet-tool
    hadolint
    ltex-ls
    gopls
    nil
    nodePackages.bash-language-server
    nodePackages.dockerfile-language-server-nodejs
    nodePackages.typescript-language-server
    nodePackages.vscode-langservers-extracted
    (fenix.complete.withComponents [
      "cargo"
      "clippy"
      "rust-src"
      "rustc"
      "rustfmt"
    ])
    rust-analyzer-nightly
    shellcheck
    sumneko-lua-language-server
    taplo-lsp # TOML
  ];

  programs.helix = {
    enable = true;
    package = inputs.helix.packages.${pkgs.system}.default;
    # TODO: Customize; might need to update home-manager first
    # Introducing PR: https://github.com/helix-editor/helix/pull/5934
    # Instructions: https://github.com/helix-editor/helix/issues/2070
    # Settings: https://rust-analyzer.github.io/manual.html#inlay-hints
    # language.config = {
    #   rust.inlayHints = {
    #       closureReturnTypeHints.enable = "with_block";
    #   };
    # };
    languages = [
      {
        name = "markdown";
        language-servers = [
          {
            name = "ltex-ls";
          }
        ];
      }
    ];
    settings = {
      theme = "catppuccin_${user.preferences.theme}";
      keys.normal = {
        C-h = "jump_view_left";
        C-j = "jump_view_down";
        C-k = "jump_view_up";
        C-l = "jump_view_right";
        space.l = ":toggle-option lsp.display-inlay-hints";
        space.t = ":tree-sitter-subtree";
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
        whitespace.render.tab = "all";
      };
    };
  };
}
