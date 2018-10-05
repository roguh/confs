#!/bin/sh 

function pause {
    sleep 0.1 || sleep 1
}

cd $HOME

MINIMAL=false

AUTOSTART_TRAYAPPS=true
AUTOSTART_COMPOSITOR=false
AUTOSTART_PROGRAMS=true

if $MINIMAL ; then
    AUTOSTART_TRAYAPPS=false
    AUTOSTART_COMPOSITOR=false
    AUTOSTART_PROGRAMS=false
fi

BACKGROUND_IMAGE="$HOME/Photos/savoring summer by  kadir nelson.png"
BACKGROUND_COLOR='#fff6f4'

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

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

dunst.sh &

# Prevent eyestrain at 4AM
redshift.sh &

# Lock screen after 5 minutes
xautolock -detectsleep -time 5 -notify 240 -notifier backlightoff.sh -locker locker.sh &

# Set background
(pause ; xsetroot -solid "$BACKGROUND_COLOR") &
(pause ; feh --bg-max "$BACKGROUND_IMAGE") &

# Ctrl-Alt-Backspace to kill X server
(pause ; setxkbmap -option terminate:ctrl_alt_bksp) &

if command -v fortune-notify.sh ; then
    fortune-notify.sh
fi

if $AUTOSTART_TRAYAPPS ; then
    # Check for Arch package updates
    # kalu &
    
    # udisk tray icon
    udiskie --smart-tray &

    # connect android phone to linux
    kdeconnect-indicator &
fi

if $AUTOSTART_COMPOSITOR ; then
    compton &
fi

if $AUTOSTART_PROGRAMS ; then
    # Start file synchronizer and commonly used apps

    (pause ;
        i3-msg "workspace 21:comm; append_layout .config/i3/workspace-comm.json" &
        evolution &
    )

    (pause ;
        i3-msg "workspace 22:TODO" ;
        kitty hsync-unison &
        firefox & 
    )
fi
