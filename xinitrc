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
sh ~/.screenlayout/default.sh

#Sort out the Xdefaults colour scheme things, ensures all the #defines work. Otherwise, urxvt ends up all pink and horrible.
xrdb -merge ~/.Xdefaults

#I'm not American.
setxkbmap gb

#My eyes!
sh ~/.redshift

#i3 is awesome.
exec i3
