# -sample is faster than -scale or -resize
scrot ~/tmp/s.png && \
mogrify -sample 20% -flip -spread 2 -sample 500% -paint 2 ~/tmp/s.png && \
i3lock --show-failed-attempts --image=$HOME/tmp/s.png
