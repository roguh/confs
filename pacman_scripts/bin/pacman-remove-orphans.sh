#!/bin/sh
set -x
pacman -Qtdq | pacman -Rns -
