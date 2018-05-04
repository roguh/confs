#!/usr/bin/env bash

# Backup config files
backup-t580.sh &
backup-common.sh &

# Start graphical system monitor
conky -c .top_conkyrc &
conky -c .bot_conkyrc &
conky -c .clocks_conkyrc &

# Enable key bindings
xbindkeys &

# Check for Arch package updates
kalu &

# Set background
# feh --bg-fill "Pictures/mt hope, antarctica.jpeg" &
xsetroot -solid '#f87b87' &

# Prevent eyestrain at 4AM
redshift &

# Start file synchronizers
for CMD in {hsync,local}-{unison,osync} ; do
    lxterminal -l -e "bash -c $CMD" &
done

# Lock screen after 5 minutes
xautolock -detectsleep -secure -time 5 -locker locker.sh &

# Start commonly used apps
firefox &
signal-desktop &
