{
  config,
  pkgs,
  user,
  ...
}: {
  # To build compute-heavy derivations, I had to set
  # ```
  # export TMPDIR=/tmp
  # ```
  imports = [
    "${
      fetchTarball {
        url = "https://github.com/NixOS/nixos-hardware/archive/936e4649098d6a5e0762058cb7687be1b2d90550.tar.gz";
        sha256 = "sha256:06g0061xm48i5w7gz5sm5x5ps6cnipqv1m483f8i9mmhlz77hvlw";
      }
    }/raspberry-pi/4"
    ../../modules/sops.nix
    # inputs.kmonad.nixosModules.default
  ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = ["noatime"];
    };
  };

  networking = {
    hostName = "bain";
    nameservers = [
      "192.168.1.76" # Use a pi-hole when possible
      "8.8.8.8"
    ];
  };

  # Set your time zone.
  time.timeZone = "America/Chicago";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # TODO: enable sound?

  programs.zsh.enable = true; # Set zsh as the default shell for all users.
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

  environment.systemPackages = with pkgs; [
    direnv
    nix-direnv

    age
    curl
    git
    gnupg
    kitty
    killall
    nano # Nano is installed by default
    sops
    tree
    vim
    wget
    yubikey-manager
  ];

  # List services that you want to enable:
  # services.kmonad = {
  #   enable = true;
  #   package = inputs.kmonad.packages.${pkgs.system}.default;
  #   keyboards = {
  #     kinesis-advantage-pro = {
  #       name = "kinesis-advantage-pro";
  #       device = "/dev/input/by-id/usb-0c45_7403-event-kbd";
  #       defcfg = {
  #         enable = true;
  #         compose.key = null;
  #         fallthrough = true;
  #         allowCommands = false;
  #       };
  #       config = builtins.readFile ../../kmonad/kinesis-advantage-pro.kbd;
  #     };
  #   };
  # };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable GPU acceleration
  # hardware.raspberry-pi."4".fkms-3d.enable = true;

  # Enable the X11 windowing system.
  # services.xserver = {
  #   enable = true;
  # };

  # Configure keymap in X11
  # services.xserver.layout = "us";

  # DISCUSS: may belong in user configuration?
  # services.xserver.windowManager.awesome = {
  #   enable = true;
  # };
  # services.xserver.displayManager.sessionCommands = ''
  #   ${pkgs.xorg.xset}/bin/xset r rate 165 60
  #   ${pkgs.xorg.xrandr}/bin/xrandr --output HDMI-1 --mode 1920x1080
  # '';

  hardware = {
    # NOTE: untested
    # bluetooth.enable = true;
    pulseaudio.enable = true;
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
  nixpkgs.hostPlatform = "aarch64-linux";

  environment.pathsToLink = [
    # Include direnvrc in nix store.
    "/share/nix-direnv" # This file is sourced by each user's ~/.direnvrc
    "/share/zsh" # Enable zsh completion for system packages
  ];
}
