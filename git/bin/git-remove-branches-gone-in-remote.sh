#!/bin/bash
MUST_DELETE="$(git fetch -p && git branch -vv | sed 's/\* *//' | awk '/: gone]/{print $1}')"

if [ "$MUST_DELETE" = "" ]; then
  echo No branches to delete
  exit
fi

echo Press ENTER to delete:
echo "$MUST_DELETE"
read -r OK
if [[ "$OK" != "" ]] ; then
  exit 1
fi

echo "$MUST_DELETE" | xargs git branch -d
