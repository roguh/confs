#!/bin/sh

while killall polybar ; do true ; done

if type "xrandr"; then
  for m in $(xrandr --query | grep " connected" | cut -d" " -f1 | tac); do
    MONITOR=$m polybar --reload primary &
  done
else
  polybar --reload primary &
fi
