
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
export email='eric.s.crosson@gmail.com'
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

# Fortune variables
fortune_args="-s"
unset no_fortune keep_it_clean

# Operating system tests
function isArch(){
    [[ $(uname -r |grep -io "arch\\|libre") ]] && \
        return $true; return $false; }

function atWork(){
    [[ $(hostname) == orpheus ]] && \
        return $true; return $false; }

while [[ $1 == *"-"* ]]; do     # Parse arguments
    case $1 in
        --silent ) no_fortune=1 ;;
    esac; shift; done

# Source configs
declare -r api=~/.config/bash/api
source ${api}/io
for src in /etc/bashrc /usr/share/git/git-prompt.sh; do
    source ${src} 2>/dev/null # no worries if dne
done
for src in ${api}/*; do source ${src}; done

# Path setup
pathAppend ${scripts}/nix      # Enviornment variables for all systems
pathAppend ${HOME}/.cask/bin   # Emacs dependency managment
pathAppend ${scripts}/$(hostname);
[[ $- == *i* ]] && pathAppend ${classes} CDPATH

loadFile ${aliases}/global # Global aliases
case $(uname -a) in                # OS-specific settings
    *Linux* )
        isArch && loadFile ${user_init_d}/arch

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

if atWork; then                 # Work settings
    keep_it_clean=1
    loadFile ${aliases}/centtech
    pathAppend ${scripts}/centtech
fi

if [[ -z $no_fortune && $(which fortune 2>/dev/null) ]]; then
    [ -z keep_it_clean ] && fortune_args="${fortune_args}a" # NSFW?
    message $Green "$(fortune $fortune_args)\n"; fi

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

# TODO: https://krash.be/node/25
#       color PS1 based on exit code of previous command
function prompt_command() {
    case $(uname -a) in
        *Linux*|*Darwin* )
            PS1=$(\cat <<EOF
\[$status_style\]$fill\t\n\
$(
git branch &>/dev/null; [ $? -eq 0 ] &&
  echo -n $(echo $(git status) | grep "nothing to commit" &> /dev/null 2>&1; \
  [ $? -eq 0 ] && echo $Green$(__git_ps1 2>/dev/null "(%s)") || echo $IRed$(__git_ps1 2>/dev/null "{%s}"););
)\
\[$prompt_style\]${debian_chroot:+($debian_chroot)}\u@\h:$(color_path_symlinks.sh)\$\[$command_style\]
EOF
            )
            PS1="${PS1} "       # Add the space before user input
            ;;
    esac
}
PROMPT_COMMAND=prompt_command

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

# commacd - a faster way to move around (Bash 3+).
# https://github.com/shyiko/commacd
#
# ENV variables that can be used to control commacd:
#   COMMACD_CD - function to change the directory (by default commacd uses builtin cd and pwd)
#   COMMACD_NOTTY - set it to "on" when you want to suppress user input (= print multiple matches and exit)
#
# @version 0.1.0
# @author Stanley Shyiko <stanley.shyiko@gmail.com>
# @license MIT

# turn on case-insensitive search by default
shopt -s nocaseglob

_commacd_split() { echo "$1" | sed $'s|/|\\\n/|g' | sed '/^[[:space:]]*$/d'; }
_commacd_join() { local IFS="$1"; shift; echo "$*"; }
_commacd_expand() ( shopt -s extglob nullglob; local ex=($1); printf "%s\n" "${ex[@]}"; )

_command_cd() {
  local dir=$1
  if [[ -z "$COMMACD_CD" ]]; then
    builtin cd "$dir" && pwd
  else
    $COMMACD_CD "$dir"
  fi
}

# show match selection menu
_commacd_choose_match() {
  local matches=("$@")
  for i in "${!matches[@]}"; do
    printf "%s\t%s\n" "$i" "${matches[$i]}" >&2
  done
  local selection;
  read -e -p ': ' selection >&2
  if [[ -n "$selection" ]]; then
    echo -n "${matches[$selection]}"
  else
    echo -n "$PWD"
  fi
}

_commacd_forward_by_prefix() {
  local path="${*%/}/" IFS=$'\n'
  # shellcheck disable=SC2046
  local matches=($(_commacd_expand "$(_commacd_join \* $(_commacd_split "$path"))"))
  case ${#matches[@]} in
    0) echo -n "$PWD";;
    *) printf "%s\n" "${matches[@]}"
  esac
}

# jump forward (`,`)
_commacd_forward() {
  if [[ -z "$*" ]]; then return 1; fi
  local IFS=$'\n'
  local dir=($(_commacd_forward_by_prefix "$@"))
  if [[ "$COMMACD_NOTTY" == "on" ]]; then
    printf "%s\n" "${dir[@]}"
    return
  fi
  if [[ ${#dir[@]} -gt 1 ]]; then
    dir=$(_commacd_choose_match "${dir[@]}")
  fi
  _command_cd "$dir"
}

# search backward for the vcs root (`,,`)
_commacd_backward_vcs_root() {
  local dir="$PWD"
  while [[ ! -d "$dir/.git" && ! -d "$dir/.hg" && ! -d "$dir/.svn" ]]; do
    dir="${dir%/*}"
    if [[ -z "$dir" ]]; then
      echo -n "$PWD"
      return
    fi
  done
  echo -n "$dir"
}

# search backward for the directory whose name begins with $1 (`,, $1`)
_commacd_backward_by_prefix() {
  local prev_dir dir="${PWD%/*}" matches match IFS=$'\n'
  while [[ -n "$dir" ]]; do
    prev_dir="$dir"
    dir="${dir%/*}"
    matches=($(_commacd_expand "$dir/${1}*/"))
    for match in "${matches[@]}"; do
        if [[ "$match" == "$prev_dir/" ]]; then
          echo -n "$prev_dir"
          return
        fi
    done
  done
  # at this point there is still a possibility that $1 is an actual path (passed in
  # by completion or whatever), so let's check that one out
  if [[ -d "$1" ]]; then echo -n "$1"; return; fi
  # otherwise fallback to pwd
  echo -n "$PWD"
}

# replace $1 with $2 in $PWD (`,, $1 $2`)
_commacd_backward_substitute() {
  echo -n "${PWD/$1/$2}"
}

# choose `,,` strategy based on a number of arguments
_commacd_backward() {
  local dir=
  case $# in
    0) dir=$(_commacd_backward_vcs_root);;
    1) dir=$(_commacd_backward_by_prefix "$@");;
    2) dir=$(_commacd_backward_substitute "$@");;
    *) return 1
  esac
  if [[ "$COMMACD_NOTTY" == "on" ]]; then
    echo -n "${dir}"
    return
  fi
  _command_cd "$dir"
}

_commacd_backward_forward_by_prefix() {
  local dir="$PWD" path="${*%/}/" matches match IFS=$'\n'
  if [[ "${path:0:1}" == "/" ]]; then
    # assume that we've been brought here by the completion
    dir=(${path%/}*)
    printf "%s\n" "${dir[@]}"
    return
  fi
  while [[ -n "$dir" ]]; do
    dir="${dir%/*}"
    # shellcheck disable=SC2046
    matches=($(_commacd_expand "$dir/$(_commacd_join \* $(_commacd_split "$path"))"))
    case ${#matches[@]} in
      0) ;;
      *) printf "%s\n" "${matches[@]}"
         return;;
    esac
  done
  echo -n "$PWD"
}

# combine backtracking with `, $1` (`,,, $1`)
_commacd_backward_forward() {
  if [[ -z "$*" ]]; then return 1; fi
  local IFS=$'\n'
  local dir=($(_commacd_backward_forward_by_prefix "$@"))
  if [[ "$COMMACD_NOTTY" == "on" ]]; then
    printf "%s\n" "${dir[@]}"
    return
  fi
  if [[ ${#dir[@]} -gt 1 ]]; then
    dir=$(_commacd_choose_match "${dir[@]}")
  fi
  _command_cd "$dir"
}

_commacd_completion() {
  local pattern=${COMP_WORDS[COMP_CWORD]} IFS=$'\n'
  # shellcheck disable=SC2088
  if [[ "${pattern:0:2}" == "~/" ]]; then
    # shellcheck disable=SC2116
    pattern=$(echo ~/"${pattern:2}")
  fi
  local completion=($(COMMACD_NOTTY=on $1 "$pattern"))
  if [[ "$completion" == "$PWD" || "${completion// /\\ }" == "$pattern" ]]; then
    return
  fi
  # remove trailing / (if any)
  for i in "${!completion[@]}"; do
    completion[$i]="${completion[$i]%/}";
  done
  COMPREPLY=($(compgen -W "$(printf "%s\n" "${completion[@]}")" -- ''))
}

_commacd_forward_completion() {
  _commacd_completion _commacd_forward
}

_commacd_backward_completion() {
  _commacd_completion _commacd_backward
}

_commacd_backward_forward_completion() {
  _commacd_completion _commacd_backward_forward
}

alias ,=_commacd_forward
alias ,,=_commacd_backward
alias ,,,=_commacd_backward_forward

complete -o filenames -F _commacd_forward_completion ,
complete -o filenames -F _commacd_backward_completion ,,
complete -o filenames -F _commacd_backward_forward_completion ,,,

### Caveat- I have not found a way to make this code work if it is not
### installed in one's .bashrc.

# Prefixes to avoid namespace collisions
function esc_timer_start() {
    esc_timer=${esc_timer:-$SECONDS} ;}

function esc_timer_stop() {
    esc_timer_show=$(($SECONDS - $esc_timer))
    unset esc_timer ; }

# Convert integer seconds to days,HH:MM:SS
function esc_seconds_to_days() {
    printf "%ddays,%02d:%02d:%02d" $(((($1/60)/60)/24))   \
        $(((($1/60)/60)%24)) $((($1/60)%60)) $(($1%60)) | \
        sed 's/^1days/1day/;s/^0days,\(00:\)*//;s/^0//' ; }

# Install hooks where appropriate
trap_add 'esc_timer_start' DEBUG
PROMPT_COMMAND="${PROMPT_COMMAND}; esc_timer_stop"

# The command to print our calculated information
alias took='echo $(esc_seconds_to_days ${esc_timer_show})'

### End official caveat ###

function org() {
    pushd ~/org &> /dev/null
    git-sync
}
