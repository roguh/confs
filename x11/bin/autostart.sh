#!/bin/sh
cd $HOME

function pause {
    sleep 0.2 || sleep 1
}

MINIMAL=false

AUTOSTART_TRAYAPPS=true
AUTOSTART_PROGRAMS=true

AUTOSTART_COMPOSITOR=false

if $MINIMAL ; then
    AUTOSTART_TRAYAPPS=false
    AUTOSTART_COMPOSITOR=false
    AUTOSTART_PROGRAMS=false
fi

BACKGROUND_IMAGE="$HOME/Pictures/Jupiter_from_Voyager_1.jpg"
BACKGROUND_COLOR='#fff6f4'

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)

if [ -f "$HOME/.Xresources" ]; then
    xrdb -merge "$HOME/.Xresources"
fi

export SSH_AUTH_SOCK

# Enable key bindings
xbindkeys &

# Backup config files
backup-t580.sh &
backup-common.sh &

if xrandr | grep "HDMI2 connected" > /dev/null ; then
    ~/bin/xrandr-hdmi2-right-vertical
fi

# Start graphical system monitor
conky.sh

dunst.sh &

# Prevent eyestrain at 4AM
redshift.sh &

# Lock screen after 5 minutes
xautolock -detectsleep -time 5 -notify 60 -notifier backlightoff.sh -locker locker.sh &

# Set background
(xsetroot -solid "$BACKGROUND_COLOR") &
(feh --bg-max "$BACKGROUND_IMAGE") &

# Ctrl-Alt-Backspace to kill X server
(pause ; setxkbmap -option terminate:ctrl_alt_bksp) &

function start {
    if command -v "$1" ; then
        "$@"
    fi
}
start fortune-notify.sh

if $AUTOSTART_TRAYAPPS ; then
    # Check for Arch package updates
    # kalu &

    # Wifi menu
    start nm-applet &

    # udisk tray icon
    start udiskie --smart-tray &

    # connect android phone to linux
    start kdeconnect-indicator &
fi

if $AUTOSTART_COMPOSITOR ; then
    start compton &
fi

if $AUTOSTART_PROGRAMS ; then
    # Start file synchronizer and commonly used apps

    (pause ;
        i3-msg "workspace 21:comm; append_layout .config/i3/workspace-comm.json" ;
        (sleep 15; start evolution) &
        start firefox &
        start keybase-gui &
    )
fi

( pause ;
  if [ -f ~/.cache/wal/sequences ]; then
      cat ~/.cache/wal/sequences
  fi
)
