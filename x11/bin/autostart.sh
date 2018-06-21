#!/bin/sh 

cd $HOME

AUTOSTART_TRAYAPPS=true
AUTOSTART_COMPOSITOR=true
AUTOSTART_PROGRAMS=true

BACKGROUND_IMAGE="$HOME/Photos/John Bramblitt/Stroll in the Rain.jpg"
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
xautolock -detectsleep -time 10 -notify 120 -notifier backlightoff.sh -locker locker.sh &

# Set background
(sleep 1 ; feh --bg-center "$BACKGROUND_IMAGE") &
# (sleep 1 ; xsetroot -solid "$BACKGROUND_COLOR") &

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
    # Start file synchronizer and commonly used apps

    (sleep 1 ; 
        i3-msg "workspace 21:comm; append_layout .config/i3/workspace-comm.json" &
        signal.sh & 
        nheko &
    )

    (sleep 1 ; 
        i3-msg "workspace 1; append_layout .config/i3/workspace-firefox.json" &
        lxterminal -l -e "sh -c hsync-unison" &
        firefox &
    )
fi
