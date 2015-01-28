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

### Begin the applications that should begin with Xorg
delay_to_stable_xorg=2

# axe caps lock
((sleep $delay_to_stable_xorg; /home/eric/bin/linux/xmap-ctrl-nocaps.sh)) &

# TODO: put these somewhere, don't keep them here for reference
# begin xscreensaver
# ((sleep $delay_to_stable_xorg); xscreensaver -no-splash) &

# prevent RSI
# ((sleep $delay_to_stable_xorg); xwrits breaktime=3:00 typetime=57:00 +finger +breakclock) &

# expose X session to VNC
# ((sleep $delay_to_stable_xorg); x11vnc -display :0 -forever -passwd raptor -bg ) &

# begin synergy server
# ((sleep $delay_to_stable_xorg); synergys) &

# exec dwm
exec gnome-session
# exec gnome-session --session=xmonad
# exec xmonad
# exec awesome
