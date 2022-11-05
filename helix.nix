{ inputs, system, ... }:

{
  programs.helix = {
    enable = true;
    package = inputs.helix.packages.${system}.default;

    settings = {
      theme = "catppuccin_mocha";
    };
  };
}
