{pkgs, ...}: {
  home.packages = with pkgs;
    [
      evtest
    ]
    ++
    # These packages are only supported on x86_64-linux
    pkgs.lib.optionals (pkgs.system == "x86_64-linux") [simplenote];
}
