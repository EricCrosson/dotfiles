# Setup fzf
# ---------
prefix="$(brew --prefix fzf)"
if [[ ! "$PATH" == "${prefix}/bin" ]]; then
  export PATH="${PATH:+${PATH}:}${prefix}/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "${prefix}/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "${prefix}/shell/key-bindings.zsh"
