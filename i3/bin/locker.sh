#!/bin/sh -
CANCEL_WITH_MOUSE_TIMEOUT=15

backlightoff.sh &
# exec i3lock -c ff0000

timeout $CANCEL_WITH_MOUSE_TIMEOUT read-one-mouse-char.sh

if [ $? != 124 ]; then
  echo Lock canceled by mouse movement
  exit 124
fi

if command -v blurlock > /dev/null; then
  blurlock
else
  rm -f $HOME/tmp/s.png

  # -sample is faster than -scale or -resize
  scrot $HOME/tmp/s.png && \
  mogrify -sample 50% -spread 1 -paint 2 -normalize -sample 200% $HOME/tmp/s.png && \
  i3lock --ignore-empty-password --show-failed-attempts -c 111111 --image="$HOME/tmp/s.png"
fi
