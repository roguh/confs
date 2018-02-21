# -sample is faster than -scale or -resize
scrot ~/tmp/s.png && mogrify -sample 50% -spread 5 -sample 200% ~/tmp/s.png && \
i3lock --show-failed-attempts --show-failed-attempts --image=$HOME/tmp/s.png
