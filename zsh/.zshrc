#####################################################################
# config
#####################################################################

##
# General configuration
export HISTSIZE=10000                    # retain a reasonable history
zmodload zsh/terminfo                    #
##

##
# Use bash-style navigation
bindkey -e                               # use bash input mode
autoload -U select-word-style            # kill-word stops at directory delimeter
setopt interactivecomments               # bash-style comments
autoload copy-earlier-word && \
	zle -N copy-earlier-word && \
	bindkey '^[,' copy-earlier-word  # cycle through last-words with M-.
##

##
# Editor for local and remote sessions
if [[ -n "${SSH_CONNECTION}" ]]; then
  export EDITOR='vim'
else
  export EDITOR='vim'
fi
##

##
# Path configuration
path+=('/usr/local/bin')
path+=('/usr/bin')
path+=('/bin')
path+=('/usr/local/sbin')
path+=('/usr/sbin')
path+=('/sbin')
path=("${HOME}/bin" $path)
path=("${HOME}/bin/nix" $path)
path=("${HOME}/bin/darwin" $path)
path=("${HOME}/.cargo/bin" $path)  # rust
path=("${HOME}/Library/Python/2.7/bin" $path)
##

##
# Ruby configuration
path=("${HOME}/.gem/bin" $path)
[ -f ~/.travis/travis.sh ] && source ~/.travis/travis.sh
##

##
# Golang configuration
export GOPATH="${HOME}/workspace/golang"
path+=('/usr/local/go/bin')
path=("${GOPATH}/bin" $path)
##

##
# Python configuration
PATH=~/Library/Python/2.7/bin:$PATH source "$(which virtualenvwrapper.sh)" 2>/dev/null
##

# TODO: move aliases somewhere else, source in a shell-independent manner, like
# direnv (on github).
alias l='ls -lahv '
alias g='git '
alias v='vim '
alias s='screen '
alias sudo='sudo '
alias o='open '

# TODO: implementation bleeding, pull this functionality into package
[ -f "${HOME}/vault/slack-notify" ] && source "${HOME}/vault/slack-notify"

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

# dircolors on completed entries
zstyle ':completion:*' list-colors 'di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

zstyle ':completion:*:processes' command "ps -u $USER -o pid,stat,%cpu,%mem,cputime,command"

#####################################################################
# plugins
#####################################################################

##
# Configure zplug packages
export SMART_CD_ONLY_IF_FITS="true"
# export SMART_CD_LS_COMMAND="k --human"
##

##
# Bootstrap zplug
if [[ ! -d ~/.zplug ]]; then
  git clone https://github.com/zplug/zplug ~/.zplug
  source ~/.zplug/init.zsh && zplug update --self
fi
source ${HOME}/.zplug/init.zsh

zplug "zplug/zplug", hook-build:'zplug --self-manage'

zplug "frmendes/geometry", as:theme
zplug "chrissicool/zsh-256color"
zplug "jreese/zsh-titles"

# TODO: wrap in a darwin-guard
zplug "plugins/osx",   from:oh-my-zsh

zplug "plugins/docker",   from:oh-my-zsh
zplug "plugins/docker-compose",   from:oh-my-zsh
# zplug "plugins/emoji",   from:oh-my-zsh
zplug "plugins/golang",   from:oh-my-zsh
zplug "plugins/git",   from:oh-my-zsh
zplug "plugins/github",   from:oh-my-zsh
zplug "plugins/gitignore",   from:oh-my-zsh
zplug "plugins/docker",   from:oh-my-zsh
zplug "plugins/pip",   from:oh-my-zsh
zplug "plugins/colored-man-pages", from:oh-my-zsh

zplug "MichaelAquilina/zsh-you-should-use"

zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-history-substring-search"
zplug "robsis/zsh-completion-generator"

zplug "hlissner/zsh-autopair", defer:2
zplug "supercrabtree/k"
zplug "ericcrosson/smart-cd"
zplug "tarrasch/zsh-functional"

# zplug "marzocchi/zsh-notify"
# zplug "rutchkiwi/copyzshell"
# zplug "willghatch/zsh-snippets"
# zplug "oknowton/zsh-dwim"  # works but throws error
# zplug "hchbaw/zce.zsh"

# commands
zplug "peterhurford/up.zsh"
# zplug "junegunn/fzf-bin", from:gh-r, as:command, rename-to:fzf
# zplug "stedolan/jq", as:command, from:gh-r, rename-to:jq
# zplug "b4b4r07/httpstat", as:command, use:'(*).sh', rename-to:'$1'
# zplug "b4b4r07/ssh-keyreg", as:command, use:bin     # add ssh key to github
# zplug "lukechilds/zsh-better-npm-completion", defer:2

zplug "vifon/deer", use:deer                        # inspired by ranger
zle -N deer
bindkey '\ek' deer

! zplug check && zplug install
zplug load
##

#####################################################################
# post-config
#####################################################################

# if [ -e "${HOME}/.nvm/nvm.sh" ]; then
# 	export NVM_DIR="${HOME}/.nvm"
# elif [ "$(which brew)" ]; then
# 	export NVM_DIR="$(brew --prefix nvm)"
# fi
# [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"

##
# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
