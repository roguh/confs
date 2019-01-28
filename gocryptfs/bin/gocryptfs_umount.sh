#!/bin/sh
DIR="$HOME/private"

if [ "$#" -ne 1 ]; then
  for f in "$DIR"/*; do
    echo "Unmounting $f"
    fusermount -u "$f"
  done
fi

for f in "$@"; do
  echo "Unmounting $f"
  fusermount -u "$f"
done
