#!/bin/sh
# x1e /home/hugo/ $  borg init ./hugo.bak/ -e=repokey
NAME=$(date "+%Y-%m-%d")

borg create ~/hugo.bak::$NAME ~/sync/ ~/.ssh ~/src --exclude '*/node_modules' --stats

EXTERNAL="/run/media/hugo/HR STORE 1/agilemd.x1e.borg.bak1"
if [ -d "$EXTERNAL" ]; then
  borg create "$EXTERNAL"::$NAME ~/sync/ ~/.ssh ~/src --exclude '*/node_modules' --stats
fi
