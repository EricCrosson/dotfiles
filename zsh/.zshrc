#####################################################################
# config
#####################################################################

# kill-word stops at directory delimeter
autoload -U select-word-style
select-word-style bash

zmodload zsh/terminfo

# Re-compile the zgen bundle if any listed file changes on disk
ZGEN_RESET_ON_CHANGE=(${HOME}/.zshrc ${HOME}/.zshrc.local)

bindkey -e

# M-, (copy-earlier-word) cycles backward through words of the command you've
# accessed with M-. (insert-last-word)
autoload copy-earlier-word && zle -N copy-earlier-word && bindkey '^[,' copy-earlier-word

# User configuration
export PATH="$HOME/bin/nix:$HOME/bin/linux:$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:$PATH"

# rust configuration
export PATH="$HOME/.cargo/bin:$PATH"

source $(which virtualenvwrapper.sh) 2>/dev/null

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='vim'
fi

function serve {
    port="${1:-3000}"
    ruby -r webrick -e "s = WEBrick::HTTPServer.new(:Port => ${port}, :DocumentRoot => Dir.pwd); trap('INT') { s.shutdown }; s.start"
}

alias gs='git status '

# avoid submitting these commands into the shell's history
alias nautilus=' nautilus '

alias powertop='sudo powertop'

alias pdown='shutdown -h now'
alias sudo='sudo '
alias l='ls -lahv '

# pseudo programs
function bell() {
    tput smcup  # activate alternate screen
    tput civis  # invisible cursor
    for i in $(seq 1 ${1:-1}); do
        echo -e '\a'
	sleep 0.125
    done
    tput cnorm  # normal cursor
    tput rmcup  # activate alternate screen
}

[ -f $HOME/vault/slack-notify ] && source $HOME/vault/slack-notify
[ -f $HOME/vault/slack-token-quarc ] && source $HOME/vault/slack-token-quarc

#####################################################################
# completions
#####################################################################

# Enable completions
if [ -d ~/.zsh/comp ]; then
    fpath=(~/.zsh/comp $fpath)
    autoload -U ~/.zsh/comp/*(:t)
fi

zstyle ':completion:*' group-name ''
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:descriptions' format '%d'
zstyle ':completion:*:options' verbose yes
zstyle ':completion:*:values' verbose yes
zstyle ':completion:*:options' prefix-needed yes
# Use cache completion
# apt-get, dpkg (Debian), rpm (Redhat), urpmi (Mandrake), perl -M,
# bogofilter (zsh 4.2.1 >=), fink, mac_apps...
zstyle ':completion:*' use-cache true
zstyle ':completion:*:default' menu select=1
zstyle ':completion:*' matcher-list \
    '' \
    'm:{a-z}={A-Z}' \
    'l:|=* r:|[.,_-]=* r:|=* m:{a-z}={A-Z}'
# sudo completions
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
    /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin
zstyle ':completion:*' menu select
zstyle ':completion:*' keep-prefix
zstyle ':completion:*' completer _oldlist _complete _match _ignored \
    _approximate _list _history

autoload -U compinit; compinit -d ~/.zcompdump

# Original complete functions
compdef '_files -g "*.hs"' runhaskell
compdef _man w3mman
compdef _tex platex

# cd search path
cdpath=($HOME)

zstyle ':completion:*:processes' command "ps -u $USER -o pid,stat,%cpu,%mem,cputime,command"

#####################################################################
# plugins
#####################################################################

# Check if zplug is installed
if [[ ! -d ~/.zplug ]]; then
  git clone https://github.com/zplug/zplug ~/.zplug
  source ~/.zplug/init.zsh && zplug update --self
fi
source $HOME/.zplug/init.zsh

# Let zplug manage zplug
zplug "zplug/zplug"

# theme
# zplug "joel-porquet/zsh-dircolors-solarized.git"
zplug "bhilburn/powerlevel9k", use:powerlevel9k.zsh-theme

zplug "plugins/git",   from:oh-my-zsh
zplug "plugins/docker",   from:oh-my-zsh
zplug "plugins/screen",   from:oh-my-zsh
zplug "plugins/pip",   from:oh-my-zsh
zplug "plugins/sudo",   from:oh-my-zsh

zplug "marzocchi/zsh-notify"

zplug "plugins/git",   from:oh-my-zsh
zplug "chrissicool/zsh-256color"

zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-history-substring-search"
zplug "robsis/zsh-completion-generator"

zplug "tarruda/zsh-autosuggestions" # ->auto-fu

zplug "junegunn/fzf-bin", from:gh-r, as:command, rename-to:fzf

# Install plugins if there are plugins that have not been installed
if ! zplug check; then
  zplug install
fi
# Then, source plugins and add commands to $PATH
zplug load

#####################################################################
# post-config
#####################################################################

setopt auto_cd
cdpath=($HOME/workspace)

export NVM_DIR="/home/eric/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
