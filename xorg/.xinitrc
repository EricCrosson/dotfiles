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

# TODO: put these somewhere, don't keep them here for reference
# expose X session to VNC
# ((sleep $delay_to_stable_xorg); x11vnc -display :0 -forever -passwd raptor -bg ) &

# exec dwm
# exec i3
exec gnome-session
# exec xmonad
# exec awesome
