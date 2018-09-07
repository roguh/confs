#!/bin/sh
for c in ~/.conkyrc.d/*conkyrc ; do 
    sleep 1 ; conky -c $c &
done


