#!/bin/sh
cloc $(find . -path ./.git -prune -o -path ./restore-bak -prune -o -path ./backup-bak -prune -o -type f)