{pkgs, ...}: {
  # NOTE: This is really only relevant to BitGo
  home = {
    file = {
      gpg-agent = {
        target = ".gnupg/gpg-agent.conf";
        text = ''
          pinentry-program ${pkgs.pinentry-curses}/bin/pinentry-curses
          default-cache-ttl 43200
          default-cache-ttl-ssh 43200
          max-cache-ttl 43200
          max-cache-ttl-ssh 43200
        '';
      };

      scdaemon = {
        target = ".gnupg/scdaemon.conf";
        text = "disable-ccid";
      };
    };
  };
}
