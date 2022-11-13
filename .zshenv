#####################################################################
# Terminal emulator configuration
#####################################################################
export HISTSIZE=1500000                 # retain a reasonable history
# FIXME: use nix for this
export EDITOR=hx
skip_global_compinit=1

#####################################################################
# XDG configuration
#####################################################################
export XDG_DATA_HOME="${XDG_DATA_HOME:-"${HOME}/.local/share"}"

#####################################################################
# Zsh configuration
#####################################################################
fpath=("${HOME}/.local/share/zsh/site-functions" $fpath)
ZGEN_RESET_ON_CHANGE=("${HOME}/.zshrc" "${HOME}/.zshenv")
ZSH_WAKATIME_BIN=/etc/profiles/per-user/eric/bin/wakatime-cli

#####################################################################
# PS1 configuration
#####################################################################
PURE_CMD_MAX_EXEC_TIME=3
PURE_NODE_ENABLED=0
PURE_GIT_PULL=0
PURE_PROMPT_SYMBOL=";"

#####################################################################
# smart-cd configuration
#####################################################################
export SMART_CD_ONLY_IF_FITS_RATIO=66

#####################################################################
# fzf configuration
#####################################################################
FZF_ALT_C_COMMAND="fd --type d"
FZF_DEFAULT_COMMAND="fd --type f"

export FZF_ALT_C_COMMAND
export FZF_DEFAULT_COMMAND
export FZF_CTRL_T_COMMAND="${FZF_DEFAULT_COMMAND}"

#####################################################################
# Docker configuration
#####################################################################
# export DOCKER_BUILDKIT=1

#####################################################################
# Rust configuration
#####################################################################
path+=("${HOME}/.cargo/bin")

#####################################################################
# Global PATH configuration
#####################################################################
path+=('/usr/local/sbin')
path=("${HOME}/.local/bin" $path)

#####################################################################
# direnv configuration
#####################################################################
export DIRENV_LOG_FORMAT=""

#####################################################################
# Epilogue
#####################################################################
export -U PATH
export -U FPATH
