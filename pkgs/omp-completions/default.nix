{
  runCommand,
  omp,
}:
# Pre-generates omp's shell completion scripts at build time so shells load a
# static file from fpath instead of spawning omp on startup or completion
# init. HOME is isolated because omp may read user config while booting
# (mirrors the atuin init guard in profiles/eric/modules/zsh.nix).
runCommand "omp-completions" {
  nativeBuildInputs = [omp];
} ''
  export HOME=$(mktemp -d)
  mkdir -p $out/share/zsh/site-functions \
           $out/share/bash-completion/completions \
           $out/share/fish/vendor_completions.d

  omp completions zsh > $out/share/zsh/site-functions/_omp
  omp completions bash > $out/share/bash-completion/completions/omp
  omp completions fish > $out/share/fish/vendor_completions.d/omp.fish

  # Fail the build rather than ship a completion script that won't register
  grep -q '^#compdef omp' $out/share/zsh/site-functions/_omp
  test -s $out/share/bash-completion/completions/omp
  test -s $out/share/fish/vendor_completions.d/omp.fish
''
