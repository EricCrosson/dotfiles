{pkgs, ...}: {
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    tmp.cleanOnBoot = true;
  };

  boot.kernelParams = ["nomodeset"];

  networking = {
    hostName = "athens";
    networkmanager.enable = true;
    firewall.allowedTCPPorts = [22];
  };

  time.timeZone = "America/Chicago";
  i18n.defaultLocale = "en_US.UTF-8";
  console.keyMap = "us";

  environment = {
    shells = [pkgs.zsh];
    systemPackages = with pkgs; [git];
    variables = {
      SHELL = "${pkgs.zsh}/bin/zsh";
      LANG = "en_US.UTF-8";
    };
  };

  fonts.packages = with pkgs; [
    hack-font
    nerd-fonts.jetbrains-mono
  ];

  programs.zsh = {
    enable = true;
    enableCompletion = false; # home-manager runs compinit in ~/.zshrc
    enableBashCompletion = false;
    enableGlobalCompInit = false;
    promptInit = ""; # starship handles the prompt
  };

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "prohibit-password";
    };
  };

  users.users.eric = {
    isNormalUser = true;
    home = "/home/eric";
    extraGroups = ["wheel" "networkmanager"];
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM9idpkqe6Rk8pLXKhqCfL6Bc3jGMHdfDj06C0AU5P3J"
    ];
  };

  security.sudo.wheelNeedsPassword = false;

  nix = {
    package = pkgs.nixVersions.nix_2_34;

    extraOptions = ''
      experimental-features = nix-command flakes
      keep-derivations = true
      keep-outputs = true

      min-free = ${toString (2 * 1024 * 1024 * 1024)}
      max-free = ${toString (10 * 1024 * 1024 * 1024)}

      builders-use-substitutes = true
    '';

    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };

    optimise = {
      automatic = true;
      dates = ["04:15"];
    };

    settings.trusted-users = ["@wheel"];
  };

  system.stateVersion = "26.11";
}
