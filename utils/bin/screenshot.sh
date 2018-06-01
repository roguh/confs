#!/bin/sh
scrot '%Y-%m-%d_%H-%M-%S.png' -e 'mv $f ~/screenshots/' $@
