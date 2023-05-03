# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  pkgs,
  inputs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    # Must fix the username dereferencing in our secrets-to-file config
    ../../modules/sops.nix
    inputs.kmonad.nixosModules.default
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # Setup keyfile
  boot.initrd.secrets = {
    "/crypto_keyfile.bin" = null;
  };

  networking.hostName = "corvere"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.nameservers = [
    "192.168.1.76" # Use a pi-hole when possible
    "8.8.8.8"
  ];

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp4s0.useDHCP = true;
  networking.enableIPv6 = false;
  # boot.kernel.sysctl."net.ipv6.conf.enp4s0.disable_ipv6" = true;

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
  # Required for Apple Magic Trackpad
  services.xserver.libinput.enable = true;
  services.xserver.libinput.touchpad.naturalScrolling = true;

  # Enable networking
  # networking.networkmanager.enable = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Enable sound.
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
  services.pcscd.enable = true; # For YubiKey TOTP.

  programs.zsh.enable = true; # Set zsh as the default shell for all users.
  users.defaultUserShell = pkgs.zsh;
  environment = {
    shells = [pkgs.zsh];
    sessionVariables = {
      GITHUB_TOKEN = "$(cat ${config.sops.secrets.github_token_bitgo.path})";
      JIRA_API_TOKEN = "$(cat ${config.sops.secrets.jira_token_bitgo.path})";
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.ericcrosson = {
    isNormalUser = true;
    description = "Eric Crosson";
    extraGroups = [
      "networkmanager"
      "wheel"
      "video"
      "audio"
      "camera"
      "kvm"
      "libvirtd"
      "input"
      "uinput"
    ];
    packages = [];
  };
  security.sudo.wheelNeedsPassword = false;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    direnv
    nix-direnv

    _1password-gui
    age
    arandr
    curl
    discord
    git
    gnupg
    kitty
    killall
    nano # Nano is installed by default
    slack
    sops
    tree
    vim
    wget
    xclip
    yubikey-manager
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "curses";
  };

  # List services that you want to enable:
  services.kmonad = {
    enable = true;
    package = inputs.kmonad.packages.${pkgs.system}.default;
    keyboards = {
      kinesis-advantage-pro = {
        name = "kinesis-advantage-pro";
        device = "/dev/input/by-id/usb-05f3_0007-event-kbd";
        defcfg = {
          enable = true;
          compose.key = null;
          fallthrough = true;
          allowCommands = false;
        };
        config = builtins.readFile ../../kmonad/kinesis-advantage-pro.kbd;
      };
    };
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    # videoDrivers = ["nvidia"];
  };

  # Configure keymap in X11
  services.xserver.layout = "us";

  # DISCUSS: may belong in user configuration?
  services.xserver.windowManager.awesome = {
    enable = true;
  };
  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.xorg.xset}/bin/xset r rate 165 60
  '';
  services.picom.enable = true;

  # https://discourse.nixos.org/t/warning-boot-enablecontainers-virtualisation-containers-unsupported/21249
  boot.enableContainers = false;
  virtualisation = {
    podman = {
      enable = true;

      # Create a `docker` alias for podman, to use it as a drop-in replacement
      dockerCompat = true;

      # Required for containers under podman-compose to be able to talk to each other.
      defaultNetwork.dnsname.enable = true;
    };
  };

  nix = {
    settings = {
      auto-optimise-store = true;
    };
    gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 7d";
    };
    package = pkgs.nixVersions.unstable; # Enable Nix flakes on system.
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
  };
  nixpkgs.config.allowUnfree = true;
  nixpkgs.hostPlatform = "x86_64-linux";

  environment.pathsToLink = [
    # Include direnvrc in nix store.
    "/share/nix-direnv" # This file is sourced by each user's ~/.direnvrc
    "/share/zsh" # Enable zsh completion for system packages
  ];

  # systemd.user.services.xrandr = {
  #   description = "xrandr multiple-monitor configuration";
  #   serviceConfig = {
  #     Type = "oneshot";
  #     RemainAfterExit = true;
  #     StandardOutput = "journal";
  #     ExecStart = ''
  #     '';
  #   };
  #   wantedBy = ["default.target"];
  # };

  system = {
    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    stateVersion = "21.11"; # Did you read the comment?
    autoUpgrade = {
      enable = true;
      channel = "https://nixos.org/channels/nixos-unstable";
      allowReboot = false;
    };
  };
}
