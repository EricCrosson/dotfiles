{
  config,
  pkgs,
  ...
}: {
  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    tmp.cleanOnBoot = true;
  };

  hardware.nvidia = {
    modesetting.enable = true;
    open = false;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  # GNOME needs the normal graphics stack; nomodeset disables it.
  hardware.graphics.enable = true;
  # Match macOS natural scrolling for physical mice.
  services.libinput.mouse.naturalScrolling = true;

  services = {
    xserver = {
      enable = true;
      videoDrivers = ["nvidia"];
    };
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
    openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = "prohibit-password";
      };
    };
  };
  systemd.defaultUnit = "graphical.target";

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

  users.users.eric = {
    isNormalUser = true;
    home = "/home/eric";
    extraGroups = ["wheel" "networkmanager"];
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM9idpkqe6Rk8pLXKhqCfL6Bc3jGMHdfDj06C0AU5P3J"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDU65JVLQ6wB6W0EAhw16uE1gJuyB4XcOv4u2RES6+7cG/iqAy5ViExo7EG6UmKUeTorTn874v8BjdPrZvpkfhSanYliYbycHLEpnHUcj0D3Z6YkWtbe4qHT0CcFlmOELCgSV/3WNchLWXyyxvAMqsyi96011fV2ny3tvjI7w21zIl+eqMTSSW5DRRJyl/29yDmmISfrhFA47ZcYRF9m0/dON2hkmG2haJWAxLDXDwdAVp5xydmGgDg3EzuWE+ricvZ/9JWH3MQzfS7Lmsl7Bt2KuNw1GjWJct9cbxzybS8KKgPgK3SuzbSRD6UJf2xHpyHFJxPnBP/KlxGNl5AGyyotFW7l6xEut5IgEBdKam7UPEkG2Oj320KrNkc2eWnUlOQApHE4QLkYlG59ObitokVCXUFKQxAwK6rcS9VuG3xrLAktYMfvhkFn1gzibpsKObL+Ny0siYv2t8h8tVJLrwdS30JzqHUjxzkdQHlSBg7xRT0RI5zW0m/Rlx3PcWis= ericcrosson@MBP-0954"
    ];
  };
  # GNOME on Wayland reads mouse scrolling from dconf rather than Xorg.
  home-manager.users.eric.dconf.settings = {
    "org/gnome/desktop/peripherals/mouse"."natural-scroll" = true;
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
