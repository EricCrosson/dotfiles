_final: prev: {
  git-extras = prev.git-extras.overrideAttrs (old: {
    postInstall =
      (old.postInstall or "")
      + ''
        rm -f $out/bin/git-pr
        rm -f $out/share/man/man1/git-pr.1
      '';
  });
}
