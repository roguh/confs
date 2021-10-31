#!/bin/bash
# Get root of a git project
P=$(git rev-parse --show-toplevel 2> /dev/null)
EXIT_CODE="$?"
if [ "$EXIT_CODE" -ne 0 ]; then
  exit "$EXIT_CODE"
else
  echo $P
fi

