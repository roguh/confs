#!/bin/bash
feh --auto-zoom --borderless --draw-exif --draw-filename --draw-tinted --force-aliasing --fullscreen --hide-pointer --image-bg checks --no-screen-clip --preload --scroll-step 100 --sort filename --zoom "$@"
