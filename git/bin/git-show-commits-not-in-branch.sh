#!/bin/sh
set -x
git cherry -v "${1-main}"
