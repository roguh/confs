#!/usr/bin/env bash

conky &

xbindkeys &

# Check for Arch package updates
kalu &

# Background
# feh --bg-fill "Pictures/mt hope, antarctica.jpeg" &
xsetroot -solid '#002b36' &

backup-t580.sh &
backup-common.sh &

redshift &

for CMD in {hsync,local}-{unison,osync} ; do
    lxterminal -l -e "bash -c $CMD" &
done

# Screen locker
xautolock -detectsleep -secure -time 15 -locker locker.sh &

firefox &
