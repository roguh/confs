#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
fi

# f=$HOME/bin/autostart.sh
# if [ -f $f ] && [ -x $f ]; then
#     . "$f" &
# fi
# unset f

xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto

session=${1:-i3}

case $session in
    i3|i3wm             ) exec i3;;
    # Start i3 with 25M RAM for debug logs
    i3-debug|i3wm-debug ) exec i3 --shmlog-size=26214400;;
    rio                 ) exec 9 rio -virtuals 12 -term lxterminal;;
    kde                 ) exec startkde;;
    xfce|xfce4          ) exec startxfce4;;
    flux|fluxbox        ) exec fluxbox;;
    # No known session, try to run it as command
    *                   ) exec $1;;
esac
