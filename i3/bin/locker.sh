#!/bin/sh -
scrot $HOME/tmp/s.png

# -sample is faster than -scale or -resize
mogrify -sample 5% -colorspace Gray -spread 2 -sample 2000% $HOME/tmp/s.png && \
i3lock --show-failed-attempts --image=$HOME/tmp/s.png
