zmodload zsh/terminfo

setopt autopushd
setopt appendhistory
setopt interactivecomments
setopt histfindnodups

# cycle through last-words with M-,
autoload copy-earlier-word && \
  zle -N copy-earlier-word && \
  bindkey '^[,' copy-earlier-word

# edit the current line with C-x C-e
autoload -Uz edit-command-line && \
  zle -N edit-command-line && \
  bindkey '^X^E' edit-command-line

zstyle ':completion:*' group-name ""
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:descriptions' format '%d'
zstyle ':completion:*:options' verbose yes
zstyle ':completion:*:values' verbose yes
zstyle ':completion:*:options' prefix-needed yes
zstyle ':completion:*' use-cache true             # Use cache completion
zstyle ':completion:*:default' menu select=1
zstyle ':completion:*' matcher-list \
    "" \
    'm:{a-z}={A-Z}' \
    'l:|=* r:|[.,_-]=* r:|=* m:{a-z}={A-Z}'
# sudo completions
zstyle ':completion:*:sudo:*' command-path \
  /usr/local/sbin \
  /usr/local/bin \
  /usr/sbin \
  /usr/bin \
  /sbin \
  /bin
zstyle ':completion:*' menu select
zstyle ':completion:*' keep-prefix
zstyle ':completion:*' completer \
  _oldlist \
  _complete \
  _match \
  _ignored \
  _approximate \
  _list \
  _history

zstyle ':completion:*:processes' command "ps -u $USER -o pid,stat,%cpu,%mem,cputime,command"

# configure my preferred ctrl-w behavior
export WORDCHARS=''

# Invoke GitHub Copilot for shell completions
function copilot {
  GITHUB_TOKEN="" gh copilot suggest -t shell "$@"
}

#####################################################################
# fzf config
#####################################################################

if [ -n "${commands[fzf-share]}" ]          # https://nixos.wiki/wiki/Fzf
then
  source "$(fzf-share)/key-bindings.zsh"
  source "$(fzf-share)/completion.zsh"
fi

bindkey '^X^T' fzf-file-widget
bindkey '^T' transpose-chars

#####################################################################
# Highlighting help messages
#####################################################################
# Do not colorize `-h` output because not all `-h` flags correspond to help
alias -g -- --help='--help 2>&1 | bat --language=help --style=plain'

#####################################################################
# BitGo configuration
#####################################################################

if [ -f "${HOME}/.zshrc_bitgo" ]
then
  . "${HOME}/.zshrc_bitgo"
fi
