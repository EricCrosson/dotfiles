# dotfiles

Configurations managed by [chezmoi](https://github.com/twpayne/chezmoi).

## Building GNU Emacs on MacOS

``` shell
brew install emacs-head@28 \
    --with-cocoa \
    --with-imagemagick \
    --with-no-frame-refocus \
    --with-pdumper \
    --with-xwidgets \
    --with-modern-icon-purple
```

## MacOS customizations

### Setting a faster keyboard repeat rate

Not sure of the exact timings, but this corresponds to a little under
200ms initial delay and 25ms between repeats

``` shell
defaults write NSGlobalDomain KeyRepeat -int 1
defaults write -g InitialKeyRepeat -int 13
```

## Ubuntu customizations

### Setting a faster keyboard repeat rate

``` shell
gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 12
gsettings set org.gnome.desktop.peripherals.keyboard delay 172
```
