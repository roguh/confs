#!/bin/sh
set -x
cryptsetup luksOpen /dev/xvdi manjaro
mount /dev/mapper/manjaro /home/user/manjaro/
