#!/bin/sh
docker system prune -a
pacman -Scc
rm -rf $HOME/.cache

journalctl --disk-usage
sudo journalctl --vacuum-time=3d

du -h $HOME | sort -h > $HOME/file_sizes
less file_sizes
