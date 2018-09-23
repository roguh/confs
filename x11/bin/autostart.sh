#!/bin/sh 

cd $HOME

MINIMAL=false

AUTOSTART_TRAYAPPS=true
AUTOSTART_COMPOSITOR=false
AUTOSTART_PROGRAMS=false

if $MINIMAL ; then
    AUTOSTART_TRAYAPPS=false
    AUTOSTART_COMPOSITOR=false
    AUTOSTART_PROGRAMS=false
fi

BACKGROUND_IMAGE="$HOME/Photos/savoring summer by  kadir nelson.png"
BACKGROUND_COLOR='#fff6f4'

# Backup config files
backup-t580.sh &
backup-common.sh &

if xrandr | grep "HDMI2 connected" > /dev/null ; then
    ~/bin/xrandr-hdmi2-right-vertical
fi

# Start graphical system monitor
conky.sh

# Enable key bindings
xbindkeys &

# Prevent eyestrain at 4AM
redshift.sh &

# Lock screen after 5 minutes
xautolock -detectsleep -time 5 -notify 240 -notifier backlightoff.sh -locker locker.sh &

# Set background
(sleep 1 ; xsetroot -solid "$BACKGROUND_COLOR") &
(sleep 1 ; feh --bg-max "$BACKGROUND_IMAGE") &

# Ctrl-Alt-Backspace to kill X server
(sleep 1 ; setxkbmap -option terminate:ctrl_alt_bksp) &

# polybar.sh

if $AUTOSTART_TRAYAPPS ; then
    # Check for Arch package updates
    # kalu &
    
    # udisk tray icon
    udiskie --smart-tray &
fi

if $AUTOSTART_COMPOSITOR ; then
    compton &
fi

if $AUTOSTART_PROGRAMS ; then
    # Start file synchronizer and commonly used apps

    (sleep 1 ; 
        i3-msg "workspace 21:comm; append_layout .config/i3/workspace-comm.json" &
        signal.sh & 
        nheko &
    )

    (sleep 1 ; 
        i3-msg "workspace 22:TODO; append_layout .config/i3/workspace-firefox.json" &
        lxterminal -l -e "sh -c hsync-unison" &
        steam.sh &
        firefox & 
    )
fi

if command -v fortune-notify.sh ; then
    fortune-notify.sh
fi
