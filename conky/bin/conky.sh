#!/bin/sh
killall conky
for c in ~/.conkyrc.d/*conkyrc ; do 
    sleep 1
    conky -d -c "$c" &
done

if type "xrandr"; then
  for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
    if test -f ~/.conkyrc.d/*conkyrc$m; then
      for c in ~/.conkyrc.d/*conkyrc$m ; do
        sleep 1
        conky -d -c "$c" &
      done
    fi
  done
fi
