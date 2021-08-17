# dotfiles [![Build Status](https://cloud.drone.io/api/badges/EricCrosson/dotfiles/status.svg)](https://cloud.drone.io/EricCrosson/dotfiles)

Configurations managed by [chezmoi](https://github.com/twpayne/chezmoi).

## Use

1. Install git, curl with system package manager
1. [Install brew](https://docs.brew.sh/Installation)
1. `brew install chezmoi`
1. `git clone https://github.com/EricCrosson/dotfiles ~/.local/share/chezmoi`
1. `cp ~/.local/share/chezmoi/doc/sample_chezmoi.toml ~/.config/chezmoi/chezmoi.toml`
1. `chezmoi apply`

Don't forget to populate your secrets in `~/.config/chezmoi/chezmoi.toml`.
