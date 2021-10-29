#!/bin/sh
git remote update 1>&2

UPSTREAM='@{u}'
LOCAL=$(git rev-parse @)
REMOTE=$(git rev-parse "$UPSTREAM" 2>/dev/null)
BASE=$(git merge-base @ "$UPSTREAM" 2>/dev/null)

DEBUG=${DEBUG:false}

if [ "$DEBUG" = "true" ]; then
    echo Upstream "$UPSTREAM" 1>&2
    echo Local "$LOCAL" 1>&2
    echo Remote "$REMOTE" 1>&2
    echo Base "$BASE" 1>&2
fi

if [ "$REMOTE" = "" ]; then
    echo "No remote"
elif [ "$LOCAL" = "$REMOTE" ]; then
    echo "Up-to-date"
elif [ "$LOCAL" = "$BASE" ]; then
    echo "Need to pull"
elif [ "$REMOTE" = "$BASE" ]; then
    echo "Need to push"
else
    echo "Diverged"
fi
