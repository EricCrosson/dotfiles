{
  pkgs,
  config,
  ...
}: {
  home = {
    packages = with pkgs; [
      coreutils-full
      duti
      gawk
      iina
    ];

    activation = {
      setDefaultApps = config.lib.dag.entryAfter ["writeBoundary"] ''
        ${pkgs.duti}/bin/duti -s com.colliderli.iina io.iina.mkv all
      '';
    };
  };
}
