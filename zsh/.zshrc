# woohoo, the future is now
export TERM="xterm-256color"

source $HOME/.antigen/antigen.zsh

antigen use oh-my-zsh

# favorite acolytes
antigen bundles <<EOBUNDLES
  git
  pip

  zsh-users/zsh-syntax-highlighting
EOBUNDLES

# configure theme
antigen theme bhilburn/powerlevel9k powerlevel9k

# apply configurations
antigen apply

# M-, (copy-earlier-word) cycles backward through words of the command you've
# accessed with M-. (insert-last-word)
autoload copy-earlier-word && zle -N copy-earlier-word && bindkey '^[,' copy-earlier-word

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Add wisely, as too many plugins slow down shell startup.
plugins=(git battery nyan colored-man)

# User configuration
export PATH="$HOME/bin/nix:$HOME/bin/linux:$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:$PATH"

source $(which virtualenvwrapper.sh) 2>/dev/null

# unalias d

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
