#!/bin/sh
# Get root of a git project
P=$(git rev-parse --show-toplevel 2> /dev/null)
echo $P
