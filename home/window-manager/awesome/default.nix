{
  pkgs,
  user,
  ...
}: {
  home = {
    packages = with pkgs; [
      pamixer
      pavucontrol # Graphival audio control
      playerctl
    ];

    file = {
      ".config/awesome/".source = ../../../.config/awesome;
      ".local/share/rofi/themes/catppuccin-frappe.rasi".source = ../../../.local/share/rofi/themes/catppuccin-frappe.rasi;
      ".local/share/rofi/themes/catppuccin-latte.rasi".source = ../../../.local/share/rofi/themes/catppuccin-latte.rasi;
      ".local/share/rofi/themes/catppuccin-macchiato.rasi".source = ../../../.local/share/rofi/themes/catppuccin-macchiato.rasi;
      ".local/share/rofi/themes/catppuccin-mocha.rasi".source = ../../../.local/share/rofi/themes/catppuccin-mocha.rasi;
    };
  };

  programs.rofi = {
    enable = true;
    plugins = [pkgs.rofi-calc];
    terminal = "${pkgs.kitty}/bin/kitty";
    theme = "Catppuccin ${user.preferences.theme}";
    extraConfig = {
      modi = "run,drun,window";
      icon-theme = "Oranchelo";
      show-icons = true;
      drun-display-format = "{icon} {name}";
      disable-history = false;
      hide-scrollbar = true;
      sidebar-mode = true;
    };
  };
}
