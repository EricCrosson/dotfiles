{pkgs}:
pkgs.runCommand "zsh-compinit-test" {
  nativeBuildInputs = [pkgs.zsh pkgs.coreutils];
} ''
  set -eu
  home=$PWD/home
  mkdir -p "$home"

  # A dump older than 7 days forces the weekly full-rebuild path.
  touch -t 202001010000 "$home/.zcompdump"

  HOME=$home zsh -f -c 'source ${../zsh/compinit.zsh}'

  # Invariant: the weekly rebuild byte-compiles the dump — zrecompile -p
  # writes .zcompdump.zwc next to the dump for faster subsequent reads.
  if [[ ! -f "$home/.zcompdump.zwc" ]]; then
    echo "expected byte-compiled $home/.zcompdump.zwc after full rebuild" >&2
    exit 1
  fi
  touch "$out"
''
