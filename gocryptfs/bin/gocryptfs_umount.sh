#!/bin/sh
for f in "$@"; do
  fusermount -u "$f"
done
