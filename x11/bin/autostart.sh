#!/usr/bin/env bash

# Backup config files
backup-t580.sh &
backup-common.sh &

# Start graphical system monitor
for c in ~/.*conkyrc ; do conky -c $c & done

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
    lxterminal -l -e "bash -c $CMD" &
done

# Lock screen after 5 minutes
xautolock -detectsleep -secure -time 5 -locker locker.sh &

# Start commonly used apps
firefox &
signal-desktop &

# Set background
# feh --bg-fill "Pictures/mt hope, antarctica.jpeg" &
(sleep 1 ; xsetroot -solid '#f87b87') &

