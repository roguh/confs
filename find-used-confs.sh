#!/bin/sh
find . -maxdepth 1 -type d | sed 's/\.\///' | sort | uniq > .IN_DIR
grep '^copy_confs_for' update.sh | cut -f2 -d' ' | sort | uniq > .IN_UPDATE_SH
awk 'NR==FNR{arr[$0];next} $0 in arr' .IN_DIR .IN_UPDATE_SH
rm .IN_DIR .IN_UPDATE_SH
