#!/bin/bash
MUST_DELETE="$(git fetch -p && git branch -vv | sed 's/\* *//' | awk '/: gone]/{print $1}')"


FORCE_DELETE=${FORCE_DELETE-false}
if [ "$1" = "-f" ]; then
  FORCE_DELETE=true
fi

if [ "$MUST_DELETE" = "" ]; then
  echo No branches to delete
  exit
fi

echo Press ENTER to delete:
echo "$MUST_DELETE"
read -r OK
if ! [ "$OK" = "" ] ; then
  exit 1
fi

if [ "$FORCE_DELETE" = true ]; then
  echo "$MUST_DELETE" | xargs git branch -D
else
  echo "$MUST_DELETE" | xargs git branch -d
fi
