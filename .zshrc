#####################################################################
# config
#####################################################################

# zmodload zsh/zprof

zmodload zsh/terminfo

# Use bash-style navigation
bindkey -e                               # use bash input mode
setopt interactivecomments               # bash-style comments
setopt autopushd
setopt histignorealldups
autoload copy-earlier-word && \
  zle -N copy-earlier-word && \
  bindkey '^[,' copy-earlier-word  # cycle through last-words with M-.

#####################################################################
# tab completion
#####################################################################

# FIXME: this isn't supported on your nixos yet
zstyle ':completion:*:*:git:*' script "${HOME}/.local/share/zsh/git-completion.bash"

#####################################################################
# completions
#####################################################################

# Enable completions
if [ -d ~/.zsh/comp ]; then
  fpath=("${HOME}/.zsh/comp" $fpath)
  autoload -U ~/.zsh/comp/*(:t)
fi

zstyle ':completion:*' group-name ''
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:descriptions' format '%d'
zstyle ':completion:*:options' verbose yes
zstyle ':completion:*:values' verbose yes
zstyle ':completion:*:options' prefix-needed yes
zstyle ':completion:*' use-cache true             # Use cache completion
zstyle ':completion:*:default' menu select=1
zstyle ':completion:*' matcher-list \
    '' \
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

# dircolors on completed entries
zstyle ':completion:*' list-colors 'di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

zstyle ':completion:*:processes' command "ps -u $USER -o pid,stat,%cpu,%mem,cputime,command"

#####################################################################
# plugins
#####################################################################

if [ ! -d "${HOME}/.zgen" ]; then
  git clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen"
fi
source "${HOME}/.zgen/zgen.zsh"

if ! zgen saved; then
  zgen oh-my-zsh
  zgen oh-my-zsh plugins/colored-man-pages
  zgen oh-my-zsh plugins/docker
  zgen oh-my-zsh plugins/kubectl
  zgen oh-my-zsh plugins/node
  zgen oh-my-zsh plugins/ripgrep

  zgen load esc-zsh/smart-cd
  zgen load hlissner/zsh-autopair
  zgen load jreese/zsh-titles
  zgen load lukechilds/zsh-better-npm-completion
  zgen load reegnz/jq-zsh-plugin
  zgen load robsis/zsh-completion-generator
  zgen load sobolevn/wakatime-zsh-plugin
  zgen load zsh-users/zsh-autosuggestions  # does incur a runtime slowdown, especially during paste
  zgen load zsh-users/zsh-completions
  zgen load zsh-users/zsh-history-substring-search

  # commands
  zgen load peterhurford/up.zsh

  zgen save
fi

#####################################################################
# aliases
#####################################################################

alias c='cargo'
alias d='docker'
alias git='hub'
# FIXME: not using chezmoi templates anymore
alias grip='grip --pass {{ .github.gh_token }}'
alias h='hx --vsplit'
alias j='jira'
alias l='exa -lg --git --time-style=long-iso'
alias npx='npx --no-install'
alias rg='rg --smart-case'
alias rip='rip --graveyard "${HOME}/.local/share/Trash"'
alias ssh='ssh -t '
alias vim='nvim '
alias viddy='viddy --differences'

alias ga='git add'
alias gd='git diff'
alias gdc='git diff --cached'
alias gfa='git fetch --all --prune --jobs=10'
alias g='git'
alias gs='git status'
alias gsu='git submodule update'

# Make and change directory
# Usage: mc <dir>
#
# @example
# mc new-directory
mc() {
  local namespace="${1:?"Directory must be specified"}"
  mkdir -p -- "$1" && cd -P -- "$1"
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
# jira config
#####################################################################

# Output of this command is cached below
# eval "$(jira --completion-script-zsh)"
#compdef jira
# autoload -U compinit && compinit
# autoload -U bashcompinit && bashcompinit

_jira_bash_autocomplete() {
    local cur prev opts base
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    opts=$( ${COMP_WORDS[0]} --completion-bash ${COMP_WORDS[@]:1:$COMP_CWORD} )
    COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
    return 0
}
complete -F _jira_bash_autocomplete jira
# End cached output

# source "${HOME}/.local/share/zsh/jira"

#####################################################################
# post-config
#####################################################################

eval "$(starship init zsh)"

unsetopt sharehistory
setopt appendhistory

# Output of this command is cached below
# eval "$(direnv hook zsh)"
_direnv_hook() {
  trap -- '' SIGINT;
  eval "$("/nix/store/r13cd02nqp2m7d9p20jjh9q4ykcwdizb-direnv-2.32.1/bin/direnv" export zsh)";
  trap - SIGINT;
}
typeset -ag precmd_functions;
if [[ -z "${precmd_functions[(r)_direnv_hook]+1}" ]]; then
  precmd_functions=( _direnv_hook ${precmd_functions[@]} )
fi
typeset -ag chpwd_functions;
if [[ -z "${chpwd_functions[(r)_direnv_hook]+1}" ]]; then
  chpwd_functions=( _direnv_hook ${chpwd_functions[@]} )
fi
# End cached output

# zprof
