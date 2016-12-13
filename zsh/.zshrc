# woohoo, the future is now
export TERM="xterm-256color"

if [[ ! -f $HOME/.zgen/zgen.zsh ]]; then
    git clone https://github.com/tarjoilija/zgen.git "${HOME}/.zgen"
fi
source $HOME/.zgen/zgen.zsh

# if the init scipt doesn't exist
if ! zgen saved; then
    echo "Creating a zgen save"

    # zgen oh-my-zsh

    # plugins
    zgen oh-my-zsh plugins/git
    zgen oh-my-zsh plugins/pip
    zgen oh-my-zsh plugins/sudo
    # zgen load zsh-users/zsh-syntax-highlighting

    # bulk load
    zgen loadall <<EOPLUGINS
        zsh-users/zsh-history-substring-search
        zsh-users/zsh-completions src
EOPLUGINS

    # theme
    zgen load bhilburn/powerlevel9k powerlevel9k.zsh-theme

    # save all to init script
    zgen save
fi

# Re-compile the zgen bundle if any listed file changes on disk
ZGEN_RESET_ON_CHANGE=(${HOME}/.zshrc ${HOME}/.zshrc.local)

# M-, (copy-earlier-word) cycles backward through words of the command you've
# accessed with M-. (insert-last-word)
autoload copy-earlier-word && zle -N copy-earlier-word && bindkey '^[,' copy-earlier-word

# User configuration
export PATH="$HOME/bin/nix:$HOME/bin/linux:$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:$PATH"

source $(which virtualenvwrapper.sh) 2>/dev/null

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='vim'
fi

# Set personal aliases
if [ -f ~/.fzf.zsh ]; then
    source ~/.fzf.zsh
    source ~/dotfiles/fzf/.fzfrc
    alias f='fzf'
fi

function serve {
    port="${1:-3000}"
    ruby -r webrick -e "s = WEBrick::HTTPServer.new(:Port => ${port}, :DocumentRoot => Dir.pwd); trap('INT') { s.shutdown }; s.start"
}

alias gs='git status '

# avoid submitting these commands into the shell's history
alias nautilus=' nautilus'

alias powertop='sudo powertop'

alias pdown='shutdown -h now'
alias sudo='sudo '
alias l='ls -lahv '

[ -f $HOME/vault/slack-notify ] && source $HOME/vault/slack-notify
