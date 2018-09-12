#!/bin/sh
killall conky
for c in ~/.conkyrc.d/*conkyrc ; do 
    sleep 1
    conky -d -c "$c" &
done


