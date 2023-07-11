{
  pkgs,
  user,
  inputs,
  ...
}: {
  home = {
    username = "${user.username}";
    homeDirectory = "${user.homeDirectory}";
    stateVersion = "22.05";

    packages = with pkgs; [
      inputs.ast-grep.packages.${pkgs.system}.default
      inputs.bash-barrier.packages.${pkgs.system}.default
      inputs.git-diff-regex.packages.${pkgs.system}.default
      inputs.git-disjoint.packages.${pkgs.system}.default
      inputs.git-dl.packages.${pkgs.system}.default
      inputs.git-review.packages.${pkgs.system}.default
      inputs.npm-dep-version.packages.${pkgs.system}.default
      inputs.nurl.packages.${pkgs.system}.default

      cargo-watch
    ];
  };
}
