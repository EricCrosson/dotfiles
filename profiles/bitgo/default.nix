{pkgs, ...}: {
  home = {
    packages = with pkgs; [
      amazon-ecr-credential-helper
      awscli2
      dive
      element-desktop
      go-jira
      k9s
      kubectl
      kubectx
      gpodder
      yq-go
    ];

    file = {
      ".jira.d" = {
        # I would prefer this to be true but that doesn't appear to be working right now
        recursive = false;
        source = ../../.jira.d;
      };
      # Shell
      # REFACTOR: use shellAliases
      ".zshenv_bitgo".source = ../../.zshenv_bitgo;
      ".zshrc_bitgo".source = ../../.zshrc_bitgo;
    };
  };

  programs = {
    git = {
      signing = {
        key = "0AB49222C769F13EDE3EBFB2352FBA3B4180A44A";
        signByDefault = true;
      };
    };
  };
}
