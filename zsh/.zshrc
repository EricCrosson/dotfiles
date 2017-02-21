#####################################################################
# config
#####################################################################

# export TERM="xterm-256color"

export HISTSIZE=1000

# export SMART_CD_LS_COMMAND="k --human"
export SMART_CD_ONLY_IF_FITS="true"

# kill-word stops at directory delimeter
autoload -U select-word-style
select-word-style bash

zmodload zsh/terminfo

bindkey -e

# M-, (copy-earlier-word) cycles backward through words of the command you've
# accessed with M-. (insert-last-word)
autoload copy-earlier-word && zle -N copy-earlier-word && bindkey '^[,' copy-earlier-word

# User configuration
export PATH="$HOME/bin/nix:$HOME/bin/linux:$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:$PATH"

# rust configuration
export PATH="$HOME/.cargo/bin:$PATH"

# gem configuration
export PATH="$HOME/.gem/bin:$PATH"

source $(which virtualenvwrapper.sh) 2>/dev/null

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='vim'
fi

alias gs='git status '

# FIXME: use this with the histignore variables
# avoid submitting these commands into the shell's history
alias nautilus=' nautilus '

alias powertop='sudo powertop'

alias pdown='shutdown -h now'
alias sudo='sudo '
alias l='ls -lahv '

# pseudo programs
function serve {
    port="${1:-3000}"
    ruby -r webrick -e "s = WEBrick::HTTPServer.new(:Port => ${port}, :DocumentRoot => Dir.pwd); trap('INT') { s.shutdown }; s.start"
}

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
[ -f $HOME/vault/confluent ] && source $HOME/vault/confluent
[ -f $HOME/vault/slack-token-quarc ] && source $HOME/vault/slack-token-quarc

[ -f $HOME/.nvm/nvm.sh ] && source $HOME/.nvm/nvm.sh

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

# Check if zplug is installed
if [[ ! -d ~/.zplug ]]; then
  git clone https://github.com/zplug/zplug ~/.zplug
  source ~/.zplug/init.zsh && zplug update --self
fi
source ${HOME}/.zplug/init.zsh

zplug "zplug/zplug", hook-build:'zplug --self-manage'

zplug "bhilburn/powerlevel9k", use:powerlevel9k.zsh-theme, as:theme

zplug "plugins/pip",   from:oh-my-zsh
zplug "plugins/git",   from:oh-my-zsh
zplug "plugins/sudo",   from:oh-my-zsh
zplug "plugins/docker",   from:oh-my-zsh
zplug "plugins/screen",   from:oh-my-zsh
# zplug "plugins/colored-man-pages", from:oh-my-zsh

# zplug "zsh-users/zsh-syntax-highlighting", defer:2
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-history-substring-search"
zplug "robsis/zsh-completion-generator"
zplug "tarruda/zsh-autosuggestions"                 # ->auto-fu

zplug "arzzen/calc.plugin.zsh"
zplug "chrissicool/zsh-256color"
# zplug "marzocchi/zsh-notify"
zplug "supercrabtree/k"
zplug "hlissner/zsh-autopair", defer:2
# zplug "joel-porquet/zsh-dircolors-solarized.git"
# zplug "rutchkiwi/copyzshell"
# zplug "caarlos0/git-add-remote"
# zplug "peterhurford/git-it-on.zsh"                  # open cur repo in browser
# zplug "bric3/nice-exit-code"
# zplug "srijanshetty/node.plugin.zsh", defer:2       # load node if present
# zplug "dijitalmunky/nvm-auto"                       # `nvm use $(cat .nvmrc)`
zplug "ericcrosson/smart-cd"
# zplug "willghatch/zsh-snippets"                     # snippets
zplug "peterhurford/up.zsh"
# zplug "tarrasch/zsh-colors"
# zplug "oknowton/zsh-dwim"  # works but throws error
# zplug "hcgraf/zsh-sudo"
zplug "jreese/zsh-titles"
# zplug "tarrasch/zsh-functional"
# zplug "hchbaw/zce.zsh"
# zplug "sharat87/pip-app", as:command, use:'(*).sh', rename-to:'$1'
# zplug "michaelaquilina/zsh-autoswitch-virtualenv"  # wasn't functional

zplug "junegunn/fzf-bin", from:gh-r, as:command, rename-to:fzf
zplug "b4b4r07/zsh-gomi", as:command, use:bin/gomi, on:junegunn/fzf-bin
# zplug "stedolan/jq", as:command, from:gh-r, rename-to:jq
# zplug "b4b4r07/httpstat", as:command, use:'(*).sh', rename-to:'$1'
# zplug "b4b4r07/ssh-keyreg", as:command, use:bin     # add ssh key to github
zplug "ericcrosson/fzf-git-cloner", as:command, use:cloner
# zplug "b4b4r07/pkill.sh", as:command, use:'pkill.sh', rename-to:'pk'
zplug "lukechilds/zsh-better-npm-completion", defer:2

# git secret
# zplug "sobolevn/git-secret"
# https://sobolevn.github.io/git-secret/#installation

zplug "vifon/deer", use:deer                        # inspired by ranger
zle -N deer
bindkey '\ek' deer

## screensaver
# zstyle ":morpho" screen-saver "zmandelbrot"
#                                         # select screen saver "zmorpho"; available: zmorpho, zmandelbrot, zblank, pmorpho
#                                         # this  can also be a command, e.g. "cmatrix"
# zstyle ":morpho" arguments "-s"         # arguments given to screen saver program; -s - every key press ends
# zstyle ":morpho" delay "290"            # 5 minutes before screen saver starts
# zstyle ":morpho" check-interval "60"    # check every 1 minute if to run screen saver

## emojis
# zplug "jhawthorn/fzy", as:command, rename-to:fzy, \
#     hook-build:"make && sudo make install"
# zplug "mrowa44/emojify", as:command, use:emojify
# zplug "b4b4r07/emoji-cli", on:"junegunn/fzf-bin", if:'(( $+commands[jq] ))'

# Install plugins if there are plugins that have not been installed
! zplug check && zplug install
# Then, source plugins and add commands to $PATH
zplug load

#####################################################################
# post-config
#####################################################################

setopt auto_cd
cdpath=($HOME/workspace)

export NVM_DIR="/home/eric/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
