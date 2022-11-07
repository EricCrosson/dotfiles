# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  pkgs,
  lib,
  system,
  user,
  ...
}: let
  kmonad = import ./kmonad.nix pkgs;
in {
  imports = [
    ./hardware-configuration.nix # Include the results of the hardware scan
    ./kmonad-nixos-module.nix
    ./modules/sops.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.grub.device = "/dev/nvme0n1p2";
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "chimp"; # Define your hostname.
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

  programs.zsh.enable = true; # Set zsh as the default shell for all users.
  users.defaultUserShell = pkgs.zsh;
  environment = {
    shells = [pkgs.zsh];
    sessionVariables = {
      GH_TOKEN = "$(cat ${config.sops.secrets.github_token_personal.path})";
      GITHUB_TOKEN = "$(cat ${config.sops.secrets.github_token_personal.path})";
    };
  };

  # Define a user account.
  users.users.${user} = {
    isNormalUser = true;
    home = "/home/${user}";
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
      "kmonad" # for kmonad
    ];
  };
  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = with pkgs; [
    direnv
    nix-direnv

    _1password-gui
    age
    curl
    discord
    firefox
    git
    gnupg
    kitty
    kmonad
    killall
    nano # Nano is installed by default
    pavucontrol # Graphival audio control
    sops
    tree
    vim
    wget
    xclip
    yubioath-desktop
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    videoDrivers = ["nvidia"];
    # RESUME: set the keyboard repeat rate
  };

  # Configure keymap in X11
  services.xserver.layout = "us";

  # DISCUSS: may belong in user configuration?
  services.xserver.windowManager.bspwm = {
    enable = true;
  };

  services.kmonad = {
    enable = true;
    package = kmonad;
    keyboards = {
      kinesis-advantage = {
        name = "kinesis-advantage";
        device = "/dev/input/by-id/usb-05f3_0007-event-kbd";
        defcfg = {
          enable = true;
          compose.key = null;
          fallthrough = true;
          allowCommands = false;
        };
        config = ''
          (defalias
            xtl (tap-hold-next 1000 esc lctl)    ;; tap for esc, hold for lctrl
          )

          ;; https://github.com/shofel/dotfiles/blob/e40fc87d664f61633e8ce55082fc96bcd878041a/home-nix/kmonad/kinesis_130P.kbd
          (defsrc
            esc  F1   F2   F3   F4   F5   F6   F7   F8   F9   F10  F11  F12  prnt slck pause

            =    1    2    3    4    5                   6    7    8    9    0    -
            tab  q    w    e    r    t                   y    u    i    o    p    \
            caps a    s    d    f    g                   h    j    k    l    ;    '
            lsft z    x    c    v    b                   n    m    ,    .    / rsft
                 grv  102d left rght                          up   down [    ]

                                     lctl lalt   rmet rctl
                                bspc del  home   pgup  ret spc
                                           end   pgdn
          )

          (deflayer base
            esc  F1   F2   F3   F4   F5   F6   F7   F8   F9   F10  F11  F12  prnt slck pause

            =    1    2    3    4    5                   6    7    8    9    0    -
            tab  q    w    e    r    t                   y    u    i    o    p    \
            @xtl a    s    d    f    g                   h    j    k    l    ;    '
            lsft z    x    c    v    b                   n    m    ,    .    / rsft
                 grv  lmet left rght                          down up   [    ]

                                     lctl lalt   ralt  rctl
                                bspc del  home   pgup  ret spc
                                           end   pgdn
          )
        '';
      };
    };
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

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

  environment.pathsToLink = [
    # Include direnvrc in nix store.
    "/share/nix-direnv" # This file is sourced by each user's ~/.direnvrc
    "/share/zsh" # Enable zsh completion for system packages
  ];
  nixpkgs.overlays = [
    # Enable Nix flakes with direnv.
    (self: super: {
      nix-direnv = super.nix-direnv.override {enableFlakes = true;};
    })
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
