{pkgs, ...}: {
  home = {
    packages = with pkgs; [
      coreutils-full
      gawk
      iina
    ];
  };
}
