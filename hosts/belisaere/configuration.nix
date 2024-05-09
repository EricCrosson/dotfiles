# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  pkgs,
  user,
  inputs,
  ...
}: {
  imports = [
    ./hardware-configuration.nix # Include the results of the hardware scan
    ../../modules/sops.nix
    inputs.kmonad.nixosModules.default
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.grub.device = "/dev/nvme0n1p2";
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "belisaere"; # Define your hostname.
  networking.nameservers = [
    "192.168.1.76" # Use a pi-hole when possible
    "8.8.8.8"
  ];
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp8s0.useDHCP = true;
  networking.enableIPv6 = false;
  # boot.kernel.sysctl."net.ipv6.conf.enp8s0.disable_ipv6" = true;

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Enable sound.
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
  services.pcscd.enable = true; # For YubiKey TOTP.
  # Fixes too many open files
  # https://github.com/NixOS/nixpkgs/issues/171218
  security.pam.loginLimits = [
    {
      domain = "*";
      type = "soft";
      item = "nofile";
      value = "262144";
    }
  ];

  users.defaultUserShell = pkgs.zsh;
  environment = {
    shells = [pkgs.zsh];
    sessionVariables = {
      GITHUB_TOKEN = "$(cat ${config.sops.secrets.github_token_personal.path})";
    };
  };

  # Define a user account.
  users.users.${user.username} = {
    isNormalUser = true;
    home = user.homeDirectory;
    description = "Eric Crosson";
    extraGroups = [
      "wheel" # Enable 'sudo' for the user.
      "video"
      "audio"
      "camera"
      "kvm"
      "libvirtd"
      "networkmanager"
      "input"
      "uinput"
    ];
  };
  security.sudo.wheelNeedsPassword = false;

  # REFACTOR: can we reduce duplication here?
  environment.systemPackages = with pkgs; [
    direnv
    nix-direnv

    _1password-gui
    age
    curl
    discord
    git
    gnupg
    kitty
    killall
    nano # Nano is installed by default
    sops
    tree
    vim
    wget
    xclip # DISCUSS: should xclip belong in the user-profile packages?
    yubikey-manager
  ];

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    zsh = {
      enable = true; # Set zsh as the default shell for all users.
    };
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;

  # List services that you want to enable:
  services.kmonad = {
    enable = true;
    package = inputs.kmonad.packages.${pkgs.system}.default;
    keyboards = {
      kinesis-advantage-pro = {
        name = "kinesis-advantage-pro";
        device = "/dev/input/by-id/usb-0c45_7403-event-kbd";
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
    videoDrivers = ["nvidia"];
  };

  # Configure keymap in X11
  services.xserver.xkb.layout = "us";

  # DISCUSS: may belong in user configuration?
  services.xserver.windowManager.awesome = {
    enable = true;
  };
  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.xorg.xset}/bin/xset r rate 165 60
  '';

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # https://discourse.nixos.org/t/warning-boot-enablecontainers-virtualisation-containers-unsupported/21249
  boot.enableContainers = false;
  virtualisation = {
    podman = {
      enable = true;

      # Create a `docker` alias for podman, to use it as a drop-in replacement
      dockerCompat = true;

      # Required for containers under podman-compose to be able to talk to each other.
      defaultNetwork.settings.dns_enabled = true;
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
