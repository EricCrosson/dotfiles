{pkgs, ...}: {
  home.packages = with pkgs; [
    evtest
  ];
}
