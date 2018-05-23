#!/bin/sh
if BRANCH=`git rev-parse --abbrev-ref HEAD 2>/dev/null` ; then
    STAT=`git status -s 2>/dev/null | wc -l`
    if [ "$STAT" -gt 0 ] ; then
        echo "$BRANCH ($STAT) "
    else
        echo "$BRANCH "
    fi
fi
