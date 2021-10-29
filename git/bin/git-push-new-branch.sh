#!/bin/sh
set -x
git push --set-upstream origin $(git branch --show-current)
