{
  pkgs,
  profile,
  inputs,
  ...
}: let
  catppuccinDelta = pkgs.fetchFromGitHub {
    owner = "catppuccin";
    repo = "delta";
    rev = "e9e21cffd98787f1b59e6f6e42db599f9b8ab399";
    sha256 = "sha256-04po0A7bVMsmYdJcKL6oL39RlMLij1lRKvWl5AUXJ7Q=";
  };
in {
  home.file = {
    ".config/git/allowed_signers" = {
      text = ''
        eric.s.crosson@utexas.edu ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM9idpkqe6Rk8pLXKhqCfL6Bc3jGMHdfDj06C0AU5P3J
      '';
    };

    # Add catppuccin delta theme configuration
    ".config/git/catppuccin.gitconfig".source = "${catppuccinDelta}/catppuccin.gitconfig";
  };

  programs = {
    delta = {
      enable = true;
      enableGitIntegration = true;
      options = {
        line-numbers = true;
        features = "catppuccin-${pkgs.lib.strings.toLower profile.preferences.theme}";
      };
    };

    gh = {
      enable = true;
      extensions = [
        inputs.gh-agent.packages.${pkgs.system}.gh-agent
        inputs.gh-arm.packages.${pkgs.system}.default
        inputs.gh-automerge.packages.${pkgs.system}.default
      ];
      gitCredentialHelper = {
        enable = false;
      };
      settings = {
        git_protocol = "https";
        prompt = "enabled";
        pager = "delta";
        aliases = {
          co = "pr checkout";
        };
      };
    };

    git = {
      enable = true;
      ignores = [
        ".DS_Store"
        "/.claude/settings.local.json"
        "/.direnv"
        "/.pre-commit-config.yaml"
        "/CLAUDE.local.md"
        "/scratch/"
      ];
      includes = [
        # Color theme for git diff, but makes it harder to read.
        # {
        #   path = "~/.config/git/catppuccin.gitconfig";
        # }
      ];
      settings = {
        advice = {
          skippedCherryPicks = false;
        };
        alias = {
          a = "add";
          b = "branch";
          c = "commit";
          co = "checkout";
          # https://stackoverflow.com/a/70205254
          continue = "-c core.editor=true rebase --continue";
          d = "diff";
          default-branch = "!git symbolic-ref refs/remotes/origin/HEAD | sed 's@^refs/remotes/origin/@@'";
          ds = "diff --cached";
          f = "fetch";
          l = "log --graph --pretty=format:'%C(yellow)%h%C(cyan)%d%Creset %s %C(white)- %an, %ar%Creset'";
          p = "pull";
          fwl = "push --force-with-lease";
          re = "restore";
          rs = "restore --staged";
          s = "status";
          # [c]heck[o]ut [f]uzzy
          cof = ''
            !f() { \
              git branch --no-color --sort=-committerdate --format='%(refname:short)' | fzf --header 'git checkout' | xargs git checkout
            }; f
          '';
          pr = ''
            !f() { \
              export GIT_PR_ALL=$(gh pr list | column -ts'	') && \
              export GIT_PR_MINE=$(gh pr list --author "@me" | column -ts'	') && \
              echo "$GIT_PR_ALL" | fzf \
                --prompt 'All PRs> ' \
                --header 'CTRL-T: toggle all / my PRs' \
                --bind "ctrl-t:transform:[[ \$FZF_PROMPT =~ All ]] && \
                  echo \"change-prompt(My PRs> )+reload(printenv GIT_PR_MINE)\" || \
                  echo \"change-prompt(All PRs> )+reload(printenv GIT_PR_ALL)\"" \
              | awk '{print $(NF-5)}' | xargs git checkout
            }; f
          '';
          su = "submodule update";

          exec = "!exec ";

          # After `git reset --soft HEAD~1`, commit with the same commit message
          # Source: https://stackoverflow.com/a/25930432
          recommit = "commit --reuse-message=HEAD@{1}";
        };
        branch = {
          sort = "-committerdate";
        };
        color = {
          ui = true;
          interactive = "auto";
        };
        column = {
          ui = "auto";
        };
        commit = {
          gpgSign = true;
          verbose = true;
        };
        core = {
          autocrlf = false;
          editor = "${inputs.helix.packages.${pkgs.system}.default}/bin/hx";
          fsmonitor = true;
          untrackedCache = true;
        };
        credential = {
          username = "EricCrosson";
        };
        diff = {
          algorithm = "histogram";
          colorMoved = "plain";
          mnemonicPrefix = true;
          renames = true;
        };
        fetch = {
          all = true;
          parallel = 10;
          prune = true;
        };
        github = {
          user = "${profile.email}";
        };
        gpg = {
          format = "ssh";
        };
        "gpg \"ssh\"" = {
          program = "/Applications/1Password.app/Contents/MacOS/op-ssh-sign";
          allowedSignersFile = "${profile.homeDirectory}/.config/git/allowed_signers";
        };
        init = {
          defaultBranch = "master";
        };
        merge = {
          conflictStyle = "zdiff3";
        };
        pull = {
          rebase = true;
        };
        push = {
          autoSetupRemote = true;
          default = "simple";
        };
        rebase = {
          autoSquash = true;
          autoStash = true;
          updateRefs = true;
        };
        rerere = {
          autoupdate = true;
          enabled = true;
        };
        tag = {
          gpgSign = true;
          sort = "version:refname";
        };
        user = {
          name = "Eric Crosson";
          email = "eric.s.crosson@utexas.edu";
          signingKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM9idpkqe6Rk8pLXKhqCfL6Bc3jGMHdfDj06C0AU5P3J";
        };
      };
    };
  };
}
