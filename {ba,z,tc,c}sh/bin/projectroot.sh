#!/bin/bash
# Get root of a project
# TODO add support for mercurial and other VCSs as needed
P="$(git rev-parse --show-toplevel 2> /dev/null)"
EXIT_CODE="$?"
if [ "$EXIT_CODE" -ne 0 ]; then
  exit "$EXIT_CODE"
else
  echo "$P"
fi

