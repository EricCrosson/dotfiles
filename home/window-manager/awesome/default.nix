{pkgs, ...}: {
  home = {
    packages = with pkgs; [
      pamixer
      pavucontrol # Graphival audio control
      playerctl
      rofi
    ];

    file = {
      ".config/awesome/".source = ../../../.config/awesome;
    };
  };
}
