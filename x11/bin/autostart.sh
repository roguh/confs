#!/bin/sh 

BACKGROUND_COLOR=0DDEB2

# Backup config files
backup-t580.sh &
backup-common.sh &

if xrandr | grep "HDMI2 connected" > /dev/null ; then
    ~/.screenlayout/eDPI1-HDMI2.sh &
fi

# Start graphical system monitor
for c in ~/.conkyrc.d/*conkyrc ; do (sleep 1 ; conky -c $c) & done

# if [ `xrandr | grep " connected" | wc -l` -gt 1 ] ; then
#     for c in ~/.conkyrc.d/*conkyrc-2 ; do (sleep 1 ; conky -c $c) & done
# fi

# Enable key bindings
xbindkeys &

# Check for Arch package updates
kalu &

# Prevent eyestrain at 4AM
redshift &

# udisk tray icon
udiskie --smart-tray &

# Start file synchronizers
for CMD in {hsync,local}-{unison,osync} ; do
    lxterminal -l -e "sh -c $CMD" &
done

# Lock screen after 5 minutes
xautolock -detectsleep -secure -time 5 -locker locker.sh &

# Start commonly used apps
firefox &
signal-desktop &

# Set background
# feh --bg-fill "Pictures/mt hope, antarctica.jpeg" &
(sleep 1 ; xsetroot -solid "#$BACKGROUND_COLOR") &

# Ctrl-Alt-Backspace to kill X server
(sleep 1 ; setxkbmap -option terminate:ctrl_alt_bksp) &
