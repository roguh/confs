backlightoff.sh &

# -sample is faster than -scale or -resize
scrot ~/tmp/s.png && \
mogrify -sample 10% -colorspace Gray -spread 2 -sample 1000% -paint 6 ~/tmp/s.png && \
i3lock --show-failed-attempts --image=$HOME/tmp/s.png
