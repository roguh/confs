#!/bin/sh -
backlightoff.sh &

scrot ~/tmp/s.png

# -sample is faster than -scale or -resize
mogrify -sample 5% -colorspace Gray -spread 2 -sample 2000% ~/tmp/s.png && \
i3lock --show-failed-attempts --image=$HOME/tmp/s.png
