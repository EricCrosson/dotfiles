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
    themes = {
      kitty-alabaster-dark = {
        # Diagnostics
        warning = {fg = "highlight";};
        error = {
          fg = "error-red";
          modifiers = ["bold"];
        };
        info = {fg = "definition";};
        hint = {fg = "punctuation";};
        diagnostic = {underline = {style = "curl";};};

        # UI
        "ui.background" = {bg = "bg";};
        "ui.text" = {fg = "fg";};
        "ui.text.focus" = {
          fg = "active";
          modifiers = ["bold"];
        };
        "ui.cursor" = {
          bg = "active";
          fg = "bg";
        };
        "ui.cursor.primary" = {
          bg = "active";
          fg = "bg";
        };
        "ui.cursor.match" = {bg = "highlight";};
        "ui.cursorline" = {bg = "cursorline";};
        "ui.cursorcolumn" = {bg = "cursorline";};
        "ui.gutter" = {bg = "bg";};
        "ui.selection" = {bg = "selection";};
        "ui.selection.primary" = {bg = "selection-primary";};
        "ui.linenr" = {fg = "punctuation";};
        "ui.linenr.selected" = {
          fg = "active";
          modifiers = ["bold"];
        };
        "ui.statusline" = {
          fg = "fg";
          bg = "panel";
        };
        "ui.statusline.inactive" = {
          fg = "punctuation";
          bg = "panel";
        };
        "ui.bufferline.active" = {
          fg = "fg";
          bg = "bg";
          modifiers = ["bold"];
        };
        "ui.bufferline.background" = {bg = "bg";};
        "ui.bufferline" = {
          fg = "punctuation";
          bg = "panel";
        };
        "ui.menu" = {
          fg = "fg";
          bg = "panel";
        };
        "ui.menu.selected" = {
          fg = "bg";
          bg = "active";
        };
        "ui.popup" = {
          fg = "fg";
          bg = "panel";
        };
        "ui.popup.info" = {
          fg = "fg";
          bg = "panel";
        };
        "ui.window" = {fg = "punctuation";};
        "ui.help" = {
          fg = "fg";
          bg = "bg";
        };
        "ui.virtual.jump-label" = {
          fg = "jump-label";
          modifiers = ["bold"];
        };
        "ui.virtual" = {fg = "inlay-hint";};
        "ui.virtual.inlay-hint" = {fg = "inlay-hint";};
        "ui.virtual.ruler" = {bg = "cursorline";};

        # LSP diagnostics
        "diagnostic.error" = {
          underline = {
            color = "error-red";
            style = "curl";
          };
        };
        "diagnostic.warning" = {
          underline = {
            color = "highlight";
            style = "curl";
          };
        };
        "diagnostic.info" = {
          underline = {
            color = "definition";
            style = "curl";
          };
        };
        "diagnostic.hint" = {
          underline = {
            color = "punctuation";
            style = "curl";
          };
        };
        "diagnostic.deprecated" = {
          fg = "punctuation";
          modifiers = ["crossed_out"];
        };

        # Syntax — Alabaster minimal: only 4 categories get color
        "string" = {fg = "string";};
        "string.regexp" = {fg = "string";};
        "string.special" = {fg = "string";};
        "constant" = {fg = "constant";};
        "constant.numeric" = {fg = "constant";};
        "constant.character" = {fg = "constant";};
        "constant.builtin" = {fg = "constant";};
        "comment" = {fg = "comment";};
        "comment.line" = {fg = "comment";};
        "comment.block" = {fg = "comment";};
        "function" = {fg = "definition";};
        "function.builtin" = {fg = "definition";};
        "function.method" = {fg = "definition";};
        "constructor" = {fg = "definition";};
        "type" = {fg = "definition";};
        "type.builtin" = {fg = "definition";};

        # Not highlighted (Alabaster philosophy)
        "keyword" = {fg = "fg";};
        "keyword.control" = {fg = "fg";};
        "keyword.operator" = {fg = "fg";};
        "variable" = {fg = "fg";};
        "variable.parameter" = {fg = "fg";};
        "variable.builtin" = {fg = "fg";};

        # Punctuation dimmed
        "punctuation" = {fg = "punctuation";};
        "punctuation.bracket" = {fg = "punctuation";};
        "punctuation.delimiter" = {fg = "punctuation";};
        "operator" = {fg = "punctuation";};

        # Special cases
        "tag" = {fg = "definition";};
        "tag.error" = {
          fg = "error-red";
          underline = {style = "line";};
        };
        "attribute" = {fg = "fg";};
        "namespace" = {fg = "definition";};
        "label" = {fg = "constant";};

        # Markup
        "markup.heading" = {
          fg = "definition";
          modifiers = ["bold"];
        };
        "markup.list" = {fg = "fg";};
        "markup.bold" = {modifiers = ["bold"];};
        "markup.italic" = {modifiers = ["italic"];};
        "markup.link.url" = {
          fg = "string";
          modifiers = ["underlined"];
        };
        "markup.link.text" = {fg = "definition";};
        "markup.quote" = {fg = "comment";};
        "markup.raw" = {fg = "string";};

        # Diff
        "diff.plus" = {fg = "string";};
        "diff.minus" = {fg = "error-red";};
        "diff.delta" = {fg = "highlight";};

        palette = {
          fg = "#DDDDDD";
          bg = "#000000";
          string = "#19CB00";
          constant = "#CB1ED1";
          comment = "#CC0403";
          definition = "#0D73CC";
          punctuation = "#767676";
          selection = "#2A2800";
          selection-primary = "#3D3B00";
          active = "#CCCCCC";
          highlight = "#CECB00";
          error-red = "#CC0403";
          panel = "#1A1A1A";
          cursorline = "#0D0D0D";
          jump-label = "#F2201F";
          inlay-hint = "#4A6A90";
          diff-green = "#19CB00";
          diff-red = "#CC0403";
          diff-orange = "#CECB00";
        };
      };

      kitty-alabaster-dark-bg = {
        inherits = "kitty-alabaster-dark";

        # Syntax — BG variant: background colors instead of text colors
        "string" = {
          fg = "fg";
          bg = "string-bg";
        };
        "string.regexp" = {
          fg = "fg";
          bg = "string-bg";
        };
        "string.special" = {
          fg = "fg";
          bg = "string-bg-dark";
        };
        "constant" = {fg = "constant";};
        "constant.numeric" = {fg = "constant";};
        "constant.character" = {fg = "constant";};
        "constant.builtin" = {fg = "constant";};
        "comment" = {
          fg = "fg";
          bg = "comment-bg";
        };
        "comment.line" = {
          fg = "fg";
          bg = "comment-bg";
        };
        "comment.block" = {
          fg = "fg";
          bg = "comment-bg";
        };
        "function" = {
          fg = "fg";
          bg = "definition-bg";
        };
        "function.builtin" = {
          fg = "fg";
          bg = "definition-bg";
        };
        "function.method" = {
          fg = "fg";
          bg = "definition-bg";
        };
        "constructor" = {
          fg = "fg";
          bg = "definition-bg";
        };
        "type" = {
          fg = "fg";
          bg = "definition-bg";
        };
        "type.builtin" = {
          fg = "fg";
          bg = "definition-bg";
        };
        "tag" = {
          fg = "fg";
          bg = "definition-bg";
        };
        "namespace" = {
          fg = "fg";
          bg = "definition-bg";
        };

        # Markup overrides
        "markup.heading" = {
          fg = "fg";
          bg = "definition-bg";
          modifiers = ["bold"];
        };
        "markup.link.url" = {
          fg = "fg";
          bg = "string-bg";
          modifiers = ["underlined"];
        };
        "markup.link.text" = {
          fg = "fg";
          bg = "definition-bg";
        };
        "markup.quote" = {
          fg = "fg";
          bg = "comment-bg";
        };
        "markup.raw" = {
          fg = "fg";
          bg = "string-bg";
        };

        # Diff overrides
        "diff.plus" = {
          fg = "fg";
          bg = "string-bg";
        };
        "diff.minus" = {
          fg = "fg";
          bg = "error-bg";
        };

        palette = {
          string-bg = "#0A1F0A";
          string-bg-dark = "#0F2A0F";
          definition-bg = "#0A1525";
          comment-bg = "#1F1F0A";
          error-bg = "#1F0A0A";
          selection-primary = "#4A4800";
          selection = "#333100";
        };
      };
    };
  };

  services.helix-theme-sync = {
    enable = true;
    settings = helixSettingsBase;
    light-theme = "wolf-alabaster-light-bg";
    dark-theme = "kitty-alabaster-dark-bg";
  };
}
