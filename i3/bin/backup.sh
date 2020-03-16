#!/bin/sh
backup-this-pacman-machine.sh
if [ -e backup-$(hostname).sh ]; then
  backup-$(hostname).sh
fi
