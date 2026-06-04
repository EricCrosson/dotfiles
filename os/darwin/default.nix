{
  pkgs,
  config,
  ...
}: {
  programs = {
    zsh = {
      profileExtra = ''
        export HOMEBREW_PREFIX="/opt/homebrew"
        export HOMEBREW_CELLAR="/opt/homebrew/Cellar"
        export HOMEBREW_REPOSITORY="/opt/homebrew"
        fpath[1,0]="/opt/homebrew/share/zsh/site-functions"
        export PATH="/opt/homebrew/bin:/opt/homebrew/sbin''${PATH+:$PATH}"
        [ -z "''${MANPATH-}" ] || export MANPATH=":''${MANPATH#:}"
        export INFOPATH="/opt/homebrew/share/info:''${INFOPATH:-}"
      '';
    };
  };

  home = {
    packages = with pkgs; [
      coreutils-full
      duti
      gawk
      iina
    ];

    activation = {
      setDefaultApps = config.lib.dag.entryAfter ["writeBoundary"] ''
        /System/Library/Frameworks/CoreServices.framework/Versions/A/Frameworks/LaunchServices.framework/Versions/A/Support/lsregister \
          -f "${config.home.homeDirectory}/Applications/Home Manager Apps/IINA.app"
        ${pkgs.duti}/bin/duti -s com.colliderli.iina .mkv all
      '';
    };
  };
}
