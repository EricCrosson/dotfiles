#####################################################################
# config
#####################################################################
# TODO: consider tangling, same as emacs.d

##
# General configuration
export HISTSIZE=10000                    # retain a reasonable history
zmodload zsh/terminfo                    #
##

##
# Use bash-style navigation
bindkey -e                               # use bash input mode
# autoload -U select-word-style            # kill-word stops at directory delimeter
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
  # TODO: set to emacs-client
  export EDITOR='vim'
fi
##

##
# fpath configuration
fpath=("${HOME}/.zsh_functions" $fpath)
##

# TODO: move aliases somewhere else, source in a shell-independent manner, like
# direnv (on github).
alias l='ls -lahv '
alias g='git '
alias v='vim '
alias s='screen '
alias sudo='sudo '
alias o='open '
alias glances='glances --theme-white '
alias ssh='ssh -t '
alias cmc='coinmarketcap '

mc() {
    mkdir -p -- "$1" &&
        cd -P -- "$1"
}

alias qutebrowser='/Applications/qutebrowser.app/Contents/MacOS/qutebrowser '
alias timer='termdown '

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
source "${HOME}/.zplug/init.zsh"

zplug "zplug/zplug", hook-build:'zplug --self-manage'

# zplug "frmendes/geometry", as:theme

PURE_CMD_MAX_EXEC_TIME=3
PURE_NODE_ENABLED=0
zplug "mafredri/zsh-async"
zplug "esc-zsh/lambda-pure"

zplug "chrissicool/zsh-256color"
zplug "jreese/zsh-titles"

export NVM_DIR="${HOME}/.local/opt/nvm"
export NVM_LAZY_LOAD=true
zplug "lukechilds/zsh-nvm"
zplug "lukechilds/zsh-better-npm-completion", defer:2

# TODO: wrap in a darwin-guard
zplug "plugins/osx",   from:oh-my-zsh

zplug "plugins/docker",   from:oh-my-zsh
zplug "plugins/docker-compose",   from:oh-my-zsh
zplug "plugins/golang",   from:oh-my-zsh
zplug "plugins/git",   from:oh-my-zsh
zplug "plugins/github",   from:oh-my-zsh
zplug "plugins/gitignore",   from:oh-my-zsh
zplug "plugins/docker",   from:oh-my-zsh
zplug "plugins/pip",   from:oh-my-zsh
zplug "plugins/colored-man-pages", from:oh-my-zsh

# TODO: can this be defered?
zplug "MichaelAquilina/zsh-you-should-use"

zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-autosuggestions"  # does incur a slowdown, esp during paste
zplug "zsh-users/zsh-history-substring-search"
zplug "robsis/zsh-completion-generator"

zplug "hlissner/zsh-autopair", defer:2
zplug "supercrabtree/k"
zplug "ericcrosson/smart-cd"
zplug "tarrasch/zsh-functional"

# zplug "esc-zsh/zsh-dirnav"  # useless on osx because ctrl-right/left already bound

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

zplug "vifon/deer", use:deer                        # inspired by ranger
zle -N deer
bindkey '\ek' deer

! zplug check && zplug install
zplug load
##

export DOCKER_BUILDKIT=1

##
# TODO: isolatte
# Tiny Care-Terminal
# List of accounts to read the last tweet from, comma separated
# # The first in the list is read by the party parrot.
export TTC_BOTS='tinycarebot,selfcare_bot,magicrealismbot'
#
# # Use this to have a different animal say a message in the big box.
# export TTC_SAY_BOX='cat'
#
# # List of folders to look into for `git` commits, comma separated.
export TTC_REPOS='~/.emacs.d,~/dotfiles,~/workspace,~/workspace/golang/src'
#
# # The max directory-depth to look for git repositories in
# # the directories defined with `TTC_REPOS`. Note that the deeper
# # the directory depth, the slower the results will be fetched.
export TTC_REPOS_DEPTH=3
#
# # Which method is to be used to read the git commits ('gitstandup' | 'gitlog').
# # If you're having problems seeing your commits in the dahsboard, set
# # this value to gitlog.
export TTC_GITBOT='gitlog'
#
# # Location/zip code to check the weather for. Both 90210 and "San Francisco, CA"
# # _should_ be ok (the zip code doesn't always work -- use a location
# # first, if you can). It's using weather.service.msn.com behind the curtains.
export TTC_WEATHER='Austin'
#
# # Set to false if you're an imperial lover <3
export TTC_CELSIUS=false
#
# # Unset this if you _don't_ want to use Twitter keys and want to
# # use web scraping instead.
export TTC_APIKEYS=true
#
# # Refresh the dashboard every 20 minutes.
export TTC_UPDATE_INTERVAL=20
#
# # Turn off terminal title
# export TTC_TERMINAL_TITLE=false
#
[ -f "${HOME}/vault/tiny-care-terminal" ] && source "${HOME}/vault/tiny-care-terminal"
##

#####################################################################
# post-config
#####################################################################

export -U PKG_CONFIG_PATH=$PKG_CONFIG_PATH:/opt/X11/lib/pkgconfig
eval "$(direnv hook zsh)"
