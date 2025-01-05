{
  pkgs,
  inputs,
  ...
}: {
  options = {
    channel = pkgs.lib.mkOption {
      default = true;
      type = pkgs.lib.types.enum ["nightly" "precompiled-release"];
      description = "Control compute requirements by selecting whether to build the latest nightly versions or use precompiled releases";
    };
  };

  config = {
    home.packages = with pkgs; [
      gopls
      hadolint
      jsonnet-language-server
      ltex-ls
      nil
      nodePackages.bash-language-server
      nodePackages.dockerfile-language-server-nodejs
      nodePackages.typescript-language-server
      nodePackages.vscode-langservers-extracted
      (fenix.complete.withComponents [
        "cargo"
        "clippy"
        "rustc"
        "rustfmt"
        "rust-src"
      ])
      # DISCUSS: in lightweight environments, using a pre-compiled rust-analyzer
      # would save a lot of time compiling
      rust-analyzer-nightly
      shellcheck
      # sumneko-lua-language-server # temporarily disabled because dependency `ilist` is marked as broken
      taplo-lsp # TOML
    ];

    programs.helix = {
      enable = true;
      # TODO: switch between channel
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
      languages.language = [
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
        theme = "everforest_light";
        keys.normal = {
          C-h = "jump_view_left";
          C-j = "jump_view_down";
          C-k = "jump_view_up";
          C-l = "jump_view_right";
          space.l = ":toggle-option lsp.display-inlay-hints";
          space.o = ":reflow";
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
  };
}
