{
  pkgs,
  profile,
  inputs,
  ...
}: let
  inherit (pkgs) stdenv;
  darwinImports = [
    ../../os/darwin
  ];
  linuxImports = [
    ../../os/linux
  ];
  fzfAltCCommand = pkgs.writeShellScript "fzf-alt-c-command" ''
    { zoxide query -l 2>/dev/null; fd --type d --absolute-path; } | awk -v home="$HOME" '!seen[$0]++ { sub("^" home, "~"); print }'
  '';
in {
  imports =
    [
      ./modules/git.nix
      ./modules/kitty.nix
      ./modules/shell-tools.nix
      ./modules/zsh.nix
    ]
    ++ (
      if stdenv.isDarwin
      then darwinImports
      else linuxImports
    );

  home = {
    username = "${profile.username}";
    homeDirectory = "${profile.homeDirectory}";
    stateVersion = "22.05";

    sessionVariables = {
      EDITOR = "${inputs.helix.packages.${pkgs.system}.default}/bin/hx";
      FZF_ALT_C_COMMAND = "${fzfAltCCommand}";
      FZF_DEFAULT_COMMAND = "fd --type f";
      FZF_CTRL_T_COMMAND = "fd --type f";
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
      MANROFFOPT = "-c";
      # Though home-manager [sets] this environment variable, it isn't
      # sourced by Xorg and awesome-wm for some reason (possibly [this
      # one]).
      #
      # [sets]: https://github.com/nix-community/home-manager/blob/ee5673246de0254186e469935909e821b8f4ec15/modules/programs/ripgrep.nix#L38
      # [this one]: https://github.com/nix-community/home-manager/issues/1011
      # TODO: test if RIPGREP_CONFIG_PATH is set in darwin without this line
      RIPGREP_CONFIG_PATH = "${profile.homeDirectory}/.config/ripgrep/ripgreprc";
      SMART_CD_ONLY_IF_FITS_RATIO = 66;
      ZSH_WAKATIME_BIN = "/etc/profiles/per-user/${profile.username}/bin/wakatime-cli";
    };

    packages = with pkgs; [
      inputs.atlas.packages.${pkgs.system}.default
      inputs.cortex.packages.${pkgs.system}.default
      inputs.bell.packages.${pkgs.system}.default
      inputs.retry.packages.${pkgs.system}.default

      age-plugin-yubikey
      amber
      atuin-desktop
      bottom
      broot
      comma
      curl
      dust
      entr
      fd
      ffmpeg

      fx
      git
      git-absorb
      git-extras
      gnupg
      gron
      htmlq
      htop
      hyperfine
      imagemagick
      jq
      moreutils
      mprocs
      pass
      pueue
      sd
      spacer
      units
      viddy
      viu
      vim
      watchexec
      wget
      yubikey-manager

      # yt-dlp # derivation temporarily broken

      # for shell
      eza
      fzf
      python3
      starship
      wakatime-cli
    ];

    file = {
      ".config/fd/ignore" = {
        text = ''
          .direnv/
          .git/
          build/
          dist/
          node_modules/
          target/
        '';
      };
    };
  };

  programs = {
    home-manager.enable = true; # Let Home Manager install and manage itself.
  };

  xdg.userDirs = {
    createDirectories = true;
    desktop = "${profile.homeDirectory}/tmp";
    download = "${profile.homeDirectory}/tmp";
    documents = "${profile.homeDirectory}/files";
    music = "${profile.homeDirectory}/files/media";
    pictures = "${profile.homeDirectory}/files/media";
    videos = "${profile.homeDirectory}/files/media";
    extraConfig = {
      XDG_DATA_HOME = "${profile.homeDirectory}/.local/share";
    };
  };
}
