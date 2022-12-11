{
  pkgs,
  user,
  inputs,
  ...
}: {
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
    rust-analyzer
    shellcheck
    sumneko-lua-language-server
    taplo-lsp # TOML
  ];

  programs.helix = {
    enable = true;
    package = inputs.helix.packages.${pkgs.system}.default;
    languages = [
      {
        name = "markdown";
        language-server.command = "ltex-ls";
      }
    ];
    settings = {
      theme = "catppuccin_${user.preferences.theme}";
      keys.normal = {
        C-h = "jump_view_left";
        C-j = "jump_view_down";
        C-k = "jump_view_up";
        C-l = "jump_view_right";
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
        whitespace.render.tab = "all";
      };
    };
  };
}
