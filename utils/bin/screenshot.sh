#!/bin/sh
DIR=$HOME/screenshots
mkdir -p "$DIR"
scrot '%Y-%m-%d_%H-%M-%S.png' -e "mv \$f $DIR" $@
