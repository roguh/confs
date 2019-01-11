#!/bin/sh
dir=$(realpath $1)
name=$(basename $dir)
mountpoint="$HOME/private/$name"
echo $dir
echo $mountpoint
mkdir $mountpoint
gocryptfs $dir $mountpoint
