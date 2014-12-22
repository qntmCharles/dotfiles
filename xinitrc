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

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice

#Setup the displays.
xrandr --output DisplayPort-1 --mode 1920x1080 --pos 0x0 --rotate left --output DisplayPort-0 --off --output DVI-1 --off --output DVI-0 --mode 1920x1080 --pos 1080x432 --rotate normal --output HDMI-0 --off

#Sort out the Xdefaults colour scheme things, ensures all the #defines work. Otherwise, urxvt ends up all pink and horrible.
xrdb -merge ~/.Xdefaults

#I'm not American.
setxkbmap gb

#i3 is awesome.
exec i3
