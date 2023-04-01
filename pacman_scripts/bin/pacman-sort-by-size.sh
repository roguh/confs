#!/bin/sh
set -x
pacman -Qi | grep -E '^(Name|Installed)' | cut -f2 -d':' | paste - - | column -t | sort -nk 2 | grep MiB
