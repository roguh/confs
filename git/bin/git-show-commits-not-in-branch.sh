#!/bin/bash
set -x
git cherry -v "${1-main}"
