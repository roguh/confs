#!/bin/sh
F=$HOME/.screenlayout/$(hostname).sh

echo $F
if [ -e $F ]
then
  echo loading $F
  $F
  sleep 1
  conky.sh
fi

