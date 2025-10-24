# ---- Fast completion + weekly rebuild + fzf-tab -----------------------------
# Single-user macOS: use cached comp map daily; do a full rebuild once/week.

# Where zsh stores the compiled completion map
typeset -g compdump="${ZDOTDIR:-$HOME}/.zcompdump"

# Load the completion system
autoload -Uz compinit

# Decide whether to rebuild (≥ 7 days old) or use cached map
# Rationale:
#   - `compinit -C` = trust cache
#   - Weekly full `compinit` keeps things sane after plugin/homebrew updates
{
  # Compute "rebuild?" using file mtime
  local -i rebuild=0 now mtime=0
  zmodload zsh/datetime 2>/dev/null
  now=${EPOCHSECONDS:-$(date +%s)}
  if [[ ! -e $compdump ]]; then
    rebuild=1
  else
    mtime=$(date -r "$compdump" +%s 2>/dev/null || echo 0)
    (( now - mtime >= 7*24*3600 )) && rebuild=1
  fi

  if (( rebuild )); then
    # Full audit + map build (slow path, but only weekly)
    compinit -d "$compdump"
    # Byte-compile for faster subsequent reads
    (( $+commands[zrecompile] )) && zrecompile -q -p "$compdump" 2>/dev/null
  else
    # Fast path: trust cache, skip audits
    compinit -C -d "$compdump"
  fi
}
