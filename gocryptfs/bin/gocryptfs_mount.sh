#!/bin/sh
set -x
PREFIX_DEST="$HOME/private"
if [ "$#" -ne 1 ]; then
  echo "USAGE: $0 ARG"
  echo "mounts ARG to $PREFIX_DEST/ARG"
  exit 1
fi
dir=$(realpath "$1")
name=$(basename "$dir")
mountpoint="$PREFIX_DEST/$name"
echo "$dir"
echo "$mountpoint"
mkdir -p "$mountpoint"
gocryptfs "$dir" "$mountpoint"
