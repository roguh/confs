#!/bin/sh 

AUTOSTART_TRAYAPPS=false
AUTOSTART_COMPOSITOR=true
AUTOSTART_PROGRAMS=false

BACKGROUND_IMAGE="Photos/thomas-kelley-194243-unsplash.jpg"
BACKGROUND_COLOR='#fff6f4'

# Backup config files
backup-t580.sh &
backup-common.sh &

if xrandr | grep "HDMI2 connected" > /dev/null ; then
    ~/.screenlayout/eDPI1-HDMI2-rotated.sh &
fi

# Start graphical system monitor
for c in ~/.conkyrc.d/*clock* ; do (sleep 1 ; conky -c $c) & done

# Enable key bindings
xbindkeys &

# Prevent eyestrain at 4AM
redshift &

# Lock screen after 5 minutes
xautolock -detectsleep -secure -time 10 -notify 120 -notifier backlightoff.sh -locker locker.sh &

# Set background
# Photo by Thomas Kelley on Unsplash
# (sleep 1 ; feh --bg-scale "$BACKGROUND_IMAGE")
# (sleep 1 ; xsetroot -solid "$BACKGROUND_COLOR") &
feh --bg-center T580.png --image-bg #ffffff

# Ctrl-Alt-Backspace to kill X server
(sleep 1 ; setxkbmap -option terminate:ctrl_alt_bksp) &

# polybar.sh

if $AUTOSTART_TRAYAPPS ; then
    # Check for Arch package updates
    kalu &
    
    # udisk tray icon
    udiskie --smart-tray &
fi

if $AUTOSTART_COMPOSITOR ; then
    compton &
fi

if $AUTOSTART_PROGRAMS ; then
    # Start file synchronizers
    lxterminal -l -e "sh -c hsync-unison" &
    
    # Start commonly used apps
    firefox &
    signal-desktop &
fi

killall i3bar &
