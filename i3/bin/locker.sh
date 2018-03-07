# -sample is faster than -scale or -resize
scrot ~/tmp/s.png && \
mogrify -sample 50% -colorspace Gray -spread 1 -sample 200% -paint 3 ~/tmp/s.png && \
i3lock --show-failed-attempts --image=$HOME/tmp/s.png
