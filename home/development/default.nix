{
  config,
  pkgs,
  inputs,
  ...
}: {
  options = {
    develops = pkgs.lib.mkOption {
      default = false;
      type = pkgs.lib.types.bool;
      description = "enable development tools";
    };
  };

  config = pkgs.lib.mkIf config.develops {
    home = {
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
  };
}
