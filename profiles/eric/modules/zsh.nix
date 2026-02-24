{
  pkgs,
  inputs,
  ...
}: let
  # Pre-generate tool init scripts at nix build time to avoid subprocess
  # spawning on every shell startup (~143ms saved)
  starshipInitZsh =
    pkgs.runCommand "starship-init-zsh" {
      nativeBuildInputs = [pkgs.starship pkgs.gnused];
    } ''
      export STARSHIP_CONFIG=/dev/null
      starship init zsh > $out
      # Pre-compute PROMPT2 to avoid a subprocess on every shell startup
      prompt2=$(starship prompt --continuation)
      sed -i "s|^PROMPT2=.*|PROMPT2=\"$prompt2\"|" $out
    '';
  direnvInitZsh =
    pkgs.runCommand "direnv-init-zsh" {
      nativeBuildInputs = [pkgs.direnv];
    } ''
      direnv hook zsh > $out
    '';
  atuinInitZsh =
    pkgs.runCommand "atuin-init-zsh" {
      nativeBuildInputs = [pkgs.atuin];
    } ''
      export HOME=$(mktemp -d)
      export ATUIN_CONFIG_DIR=$HOME/.config/atuin
      mkdir -p $ATUIN_CONFIG_DIR
      atuin init zsh --disable-up-arrow > $out
    '';
  atlasInitZsh =
    pkgs.runCommand "atlas-init-zsh" {
      nativeBuildInputs = [inputs.atlas.packages.${pkgs.system}.default];
    } ''
      atlas init zsh > $out
    '';
  zoxideInitZsh =
    pkgs.runCommand "zoxide-init-zsh" {
      nativeBuildInputs = [pkgs.zoxide];
    } ''
      zoxide init zsh > $out
    '';

  # Plugin sources — extracted so we can manage fpath and sourcing manually
  # with zsh-defer instead of letting home-manager source them synchronously
  pluginSrcs = {
    smart-cd = pkgs.fetchFromGitHub {
      owner = "esc-zsh";
      repo = "smart-cd";
      rev = "57051138141179c293dcaef2da659e42ad4f9eeb";
      sha256 = "sha256-TgWwvJqQvIjRXpYuSVZ4ZqJCqLF7a5IIqLPzyYNWaTs=";
    };
    zsh-autopair = pkgs.fetchFromGitHub {
      owner = "hlissner";
      repo = "zsh-autopair";
      rev = "396c38a7468458ba29011f2ad4112e4fd35f78e6";
      sha256 = "sha256-PXHxPxFeoYXYMOC29YQKDdMnqTO0toyA7eJTSCV6PGE=";
    };
    zsh-better-npm-completion = pkgs.fetchFromGitHub {
      owner = "lukechilds";
      repo = "zsh-better-npm-completion";
      rev = "47e5987ca422de43784f9d76311d764f82af2717";
      sha256 = "sha256-ruQZ3R0Efbe2jnw/WBvTukdtSWoX/kx2mcafnJNoN1k=";
    };
    jq-zsh-plugin = pkgs.fetchFromGitHub {
      owner = "esc-zsh";
      repo = "jq-zsh-plugin";
      rev = "205675c7fdc0a2ad3c3fab1b9bcf6d8fd0e4c585";
      sha256 = "sha256-q/xQZ850kifmd8rCMW+aAEhuA43vB9ZAW22sss9e4SE=";
    };
    wakatime-zsh-plugin = pkgs.fetchFromGitHub {
      owner = "sobolevn";
      repo = "wakatime-zsh-plugin";
      rev = "69c6028b0c8f72e2afcfa5135b1af29afb49764a";
      sha256 = "sha256-pA1VOkzbHQjmcI2skzB/OP5pXn8CFUz5Ok/GLC6KKXQ=";
    };
    up = pkgs.fetchFromGitHub {
      owner = "peterhurford";
      repo = "up.zsh";
      rev = "c8cc0d0edd6be2d01f467267e3ed385c386a0acb";
      sha256 = "sha256-yUWmKi95l7UFcjk/9Cfy/dDXQD3K/m2Q+q72YLZvZak=";
    };
    mc = pkgs.fetchFromGitHub {
      owner = "esc-zsh";
      repo = "mc";
      rev = "53f446969e5ddf8f7d0c42cdfe476203f1871414";
      sha256 = "sha256-Ll4gEV38nmtuLzu00JUDpDxm8Uq6oxDrOebio1zGV7A=";
    };
    rh = pkgs.fetchFromGitHub {
      owner = "esc-zsh";
      repo = "rh";
      rev = "2e7ba9f0e71fc7090c22e7cf1872592361296d48";
      sha256 = "sha256-vWHbPnGPNQT3VytHzy1vS63C0vl26x+5lYIumDC2ei4=";
    };
    fzf-tab = pkgs.fetchFromGitHub {
      owner = "aloxaf";
      repo = "fzf-tab";
      rev = "fac145167f7ec1861233c54de0c8900b09c650fe";
      sha256 = "sha256-1Ior+/9e+M+Fc1u0uq5HhknlGRS96q7tazhEE6rmx9Y=";
    };
    zsh-autosuggestions = pkgs.fetchFromGitHub {
      owner = "zsh-users";
      repo = "zsh-autosuggestions";
      rev = "c3d4e576c9c86eac62884bd47c01f6faed043fc5";
      sha256 = "sha256-B+Kz3B7d97CM/3ztpQyVkE6EfMipVF8Y4HJNfSRXHtU=";
    };
    zsh-completions = pkgs.fetchFromGitHub {
      owner = "zsh-users";
      repo = "zsh-completions";
      rev = "f7c3173886f4f56bf97d622677c6d46ab005831f";
      sha256 = "sha256-sZCHI4ZFfRjcG1XF/3ABf9+zv7f2Di8Xrh4Dr+qt4Us=";
    };
    zsh-history-substring-search = pkgs.fetchFromGitHub {
      owner = "zsh-users";
      repo = "zsh-history-substring-search";
      rev = "8dd05bfcc12b0cd1ee9ea64be725b3d9f713cf64";
      sha256 = "sha256-houujb1CrRTjhCc+dp3PRHALvres1YylgxXwjjK6VZA=";
    };
  };
in {
  programs.zsh = {
    enable = true;
    completionInit = ""; # deferred via zsh-defer in initContent

    history = {
      expireDuplicatesFirst = true;
      extended = true;
      ignoreAllDups = true;
      ignoreDups = false;
      share = false;
    };

    initContent = let
      p = pluginSrcs;
      brootInitZsh = "${pkgs.broot}/share/zsh/site-functions/br";
    in
      ''
        # ── fpath setup (must happen before compinit) ──────────────────────
        fpath+=(
          ${p.smart-cd}
          ${p.zsh-autopair}
          ${p.zsh-better-npm-completion}
          ${p.jq-zsh-plugin}
          ${p.wakatime-zsh-plugin}
          ${p.up}
          ${p.mc}
          ${p.rh}
          ${p.fzf-tab}
          ${p.zsh-autosuggestions}
          ${p.zsh-completions}/src
          ${p.zsh-history-substring-search}
        )

        # ── zsh-defer (must load before any deferred calls) ────────────────
        source ${pkgs.zsh-defer}/share/zsh-defer/zsh-defer.plugin.zsh

        # ── Fast initial prompt (native zsh, no subprocess) ──────────────
        # Matches starship's format: directory (blue) + newline + character (yellow)
        # Starship takes over within ~20ms via zsh-defer, adding git info
        PROMPT=$'\n%F{blue}%~%f\n%F{yellow};%f '

        # ── Synchronous autoloads and stubs (zero cost) ──────────────────
        autoload -Uz mc
        autoload -Uz rh
        up() { unfunction up; source ${p.up}/up.plugin.zsh; up "$@"; }
        br() { unfunction br; source ${brootInitZsh}; br "$@"; }

        # ── Tier 0: Functional interactivity (immediate) ─────────────────
        # Must complete before first user interaction.
        # Alt-C (95% workflow) needs direnv + zoxide; Ctrl-R (5%) needs atuin.
        zsh-defer -a source ${../../../zsh/compinit.zsh}
        zsh-defer -a source ${direnvInitZsh}
        zsh-defer -a source ${zoxideInitZsh}
        zsh-defer -a -c 'if [[ $options[zle] = on ]]; then source ${atuinInitZsh}; fi'
        zsh-defer -a source ${atlasInitZsh}
      ''
      + builtins.readFile ../../../zsh/fzf-cd-widget.zsh
      + ''
        # ── Tier 1: Visual + typing polish (after 100ms yield) ───────────
        # The -t 0.1 yields to ZLE for 100ms — shell is interactive during
        # this gap (user can press Alt-C or Ctrl-R).
        if [[ $TERM != "dumb" ]]; then
          zsh-defer -a +p -t 0.1 -c 'source ${starshipInitZsh}'
        fi
        zsh-defer -a source ${p.smart-cd}/smart-cd.plugin.zsh
        zsh-defer -a source ${p.fzf-tab}/fzf-tab.plugin.zsh
        zsh-defer -a +m +s source ${p.zsh-autosuggestions}/zsh-autosuggestions.plugin.zsh
        zsh-defer -a source ${p.zsh-autopair}/zsh-autopair.plugin.zsh
        zsh-defer -a source ${../../../zsh/deferred-shell-config.zsh}
        zsh-defer -a source ${p.zsh-history-substring-search}/zsh-history-substring-search.plugin.zsh

        # ── Tier 2: Background (after 500ms yield) ────────────────────
        # Rarely needed immediately.
        zsh-defer -a -t 0.5 source ${p.wakatime-zsh-plugin}/wakatime.plugin.zsh
        zsh-defer -a source ${p.jq-zsh-plugin}/jq.plugin.zsh
        zsh-defer -a source ${p.zsh-better-npm-completion}/zsh-better-npm-completion.plugin.zsh
      '';

    plugins = []; # plugins managed manually in initContent with zsh-defer

    sessionVariables = {
      # Configure my preferred ctrl-w behavior
      WORDCHARS = "";
    };

    setOptions = [
      "autopushd"
      "appendhistory"
      "interactivecomments"
      "histfindnodups"
      "promptsubst" # required for starship's PROMPT='$(...)' (deferred via zsh-defer)
    ];

    shellAliases = {
      g = "git";
      grip = "grip --pass $GITHUB_TOKEN";
      h = "hx --vsplit";
      l = "eza -lg --git --time-style=long-iso";
      npx = "npx --no-install";
      ssh = "ssh -t";
      viddy = "viddy --differences";
    };
  };
}
