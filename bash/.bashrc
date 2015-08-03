# BASH initialization and customizations
# Copyright (C) 2013 Eric Crosson
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples
(set -o igncr) 2>/dev/null && set -o igncr; # this comment is needed

# Shell settings
shopt -s extglob                # include extra (non-std) glob opts
shopt -s dotglob                # include dot-files in globbing
shopt -s cdspell                # fix folder spellings
shopt -s checkwinsize           # fix line wraps after resizing
shopt -s cdable_vars            # able to cd into dir described by a var
shopt -s histappend             # don't overwrite history
set -o emacs                    # set Emacs bindings
[ -t 0 ] && stty -ixon          # enable C-s history nav
bind Space:magic-space          # replace any ! symbol on space

# Exports
export email='esc@ericcrosson.com'
export EDITOR="emacsclient -ta "
export VISUAL="emacsclient -ta "
export BROWSER='chromium &2>/dev/null '
export HISTIGNORE=' *'          # ignore commands prepended with ' '
export HISTCONTROL=ignoredups   # ignore duplicates in history
export true=0
export false=1
unset MAILCHECK

# Ruby
export GEM_HOME=~/.gem
export GEM_PATH=~/.gem

# Source configs
config=$HOME/dotfiles/bash/.config/bash
declare -r api=${config}/api
source ${api}/io
for src in ${api}/*; do source ${src}; done

# Path setup
pathAppend ${scripts}/nix      # Enviornment variables for all systems
pathAppend ${scripts}/$(hostname);
[[ $- == *i* ]] && pathAppend ${classes} CDPATH

loadFile ${aliases}/global # Global aliases
case $(uname -a) in                # OS-specific settings
    *Linux* )
        pathAppend ~/.gem/ruby/2.0.0/bin
        pathAppend ${scripts}/linux
        loadFile ${aliases}/linux ;;

    *Darwin* )
        pathAppend ${scripts}/mac
        loadFile ${aliases}/mac ;;

    *Cygwin* )
        pathAppend ${scripts}/windows
        loadFile ${aliases}/win ;;
esac

# Add Ruby gem bin dir to $PATH
ruby_base=$HOME/.gem/ruby
[[ -d ${ruby_base} ]] && pathAppend ${ruby_base}/$(\ls -1 ${ruby_base} | tail -n1)/bin

# Source formatting script
case $(uname -a) in
    *Linux*|*Darwin* )
        # Reset color for command output (invoked before each command)
        case $(uname -a) in
            *Darwin* ) trap_add 'echo -ne "\033[00m"' DEBUG ;;
            *Linux* )  trap_add 'echo -ne "\e[0m"'    DEBUG ;;
        esac
        fill="--- "
        reset_style=$Color_Off
        status_style=$reset_style'\033[0;90m'  # gray; 0;37m = lighter color
        prompt_style=$reset_style
        command_style=$reset_style'\033[1;29m' # bold black
        ;;
esac

cd_func ()
{
  # TODO: document
  # TODO: add a way to cd backwards while DROPPING items instead of keeping them in the tree.
  local x2 the_new_dir adir index
  local -i cnt

  if [[ $1 ==  "--" ]]; then
    dirs -v
    return 0
  fi

  the_new_dir=$1
  [[ -z $1 ]] && the_new_dir=$HOME

  if [[ ${the_new_dir:0:1} == '-' ]]; then
    # Extract dir N from dirs
    index=${the_new_dir:1}
    [[ -z $index ]] && index=1
    adir=$(dirs +$index)
    the_new_dir=$adir
  fi

  # '~' has to be substituted by ${HOME}
  [[ ${the_new_dir:0:1} == '~' ]] && the_new_dir="${HOME}${the_new_dir:1}"

  # Now change to the new dir and add to the top of the stack
  pushd "${the_new_dir}" > /dev/null
  [[ $? -ne 0 ]] && return 1
  the_new_dir=$(pwd)

  # Trim down everything beyond 11th entry
  popd -n +11 2>/dev/null 1>/dev/null

  # Remove any other occurence of this dir, skipping the top of the stack
  for ((cnt=1; cnt <= 10; cnt++)); do
    x2=$(dirs +${cnt} 2>/dev/null)
    [[ $? -ne 0 ]] && return 0
    [[ ${x2:0:1} == '~' ]] && x2="${HOME}${x2:1}"
    if [[ "${x2}" == "${the_new_dir}" ]]; then
      popd -n +$cnt &>/dev/null
      cnt=cnt-1
    fi
  done

  return 0
}

alias cd='cd_func '
alias bd='cd - '     # Back Directory
alias sd='cd -- '    # list directories

# Lastly, load machine-specific init scripts
loadFile ${user_init_d}/$(hostname) 2>/dev/null # don't care if dne
