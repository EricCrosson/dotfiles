# nixos-rebuild-ng: drop systemd-run's --output=cat
#
# nixos-rebuild-ng switched its switch-to-configuration wrapper to
# `systemd-run --wait --verbose --output=cat`, but `--output` was only
# added to systemd-run in systemd 261. When switching *from* a running
# system with an older systemd (e.g. 260.x), the new nixos-rebuild-ng
# resolves `systemd-run` from the *running* system's PATH and dies with:
#
#   systemd-run: unrecognized option '--output=cat'
#
# The option only controls log formatting, so removing it keeps the
# switch working on both old and new systemd. Drop this overlay once
# every host runs systemd >= 261.
_final: prev: {
  nixos-rebuild-ng = prev.nixos-rebuild-ng.overrideAttrs (old: {
    postPatch =
      (old.postPatch or "")
      + ''
        if grep -q '"--output=cat",' nixos_rebuild/nix.py; then
          substituteInPlace nixos_rebuild/nix.py \
            --replace-fail '"--output=cat",' ""
        fi
      '';
  });
}
