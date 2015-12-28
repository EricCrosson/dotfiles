#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# allow external monitors while laptop is closed
gconftool-2 --type string --set /apps/gnome-power-manager/buttons/lid_ac "nothing"

# set keyboard repeat rate
xset r rate 200 60

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources
feh --bg-scale $HOME/.config/bspwm/wallpapers/wallpaper.png

# exec dwm
# exec i3
exec gnome-session
# exec xmonad
# exec awesome

sxhkd &
urxvt &
exec bspwm
