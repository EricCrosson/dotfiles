{pkgs, ...}: let
in {
  home.packages = with pkgs; [
    # Manage direnv at the system level on NixOS, which isn't available on darwin.
    direnv
  ];
}
