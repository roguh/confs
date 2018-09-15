#!/bin/sh
killall conky
for c in ~/.conkyrc.d/*conkyrc ; do 
    sleep 1
    conky -d -c "$c" &
done



if type "xrandr"; then
  for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
    for c in ~/.conkyrc.d/*conkyrc$m ; do 
      sleep 1
      conky -d -c "$c" &
    done
  done
fi
