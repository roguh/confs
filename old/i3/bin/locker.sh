#!/bin/sh -
backlightoff.sh

# scrot $HOME/tmp/s.png
# -sample is faster than -scale or -resize
# mogrify -sample 50% -spread 1 -paint 2 -normalize -sample 200% $HOME/tmp/s.png && \
i3lock --ignore-empty-password --show-failed-attempts -c 111111 --image="$HOME/Pictures/Jupiter_from_Voyager_1.jpg"
