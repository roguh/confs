#!/bin/bash -
if command -v ipython > /dev/null; then
    kitty bash -c ipython
else
    kitty bash -c python
fi
