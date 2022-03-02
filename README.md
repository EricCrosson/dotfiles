Configurations managed by [chezmoi](https://github.com/twpayne/chezmoi).

## Use

1. Install git, curl with system package manager
1. [Install brew](https://docs.brew.sh/Installation)
1. `brew install chezmoi`
1. `chezmoi init https://github.com/ericcrosson/dotfiles.git`
1. Populate secrets at `~/.config/chezmoi/chezmoi.toml`
1. `chezmoi apply`
