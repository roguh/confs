#!/bin/bash
DIR=$HOME/screenshots-$(hostname)-$(whoami)
mkdir -p "$DIR"
scrot '%Y-%m-%d_%H-%M-%S.png' -e "mv \$f $DIR" $@
