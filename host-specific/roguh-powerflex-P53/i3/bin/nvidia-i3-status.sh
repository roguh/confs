#!/bin/bash

vram=$(nvidia-smi --query-gpu=memory.used --format=csv,noheader,nounits | awk '{printf "%.2f", $1 / 1024}')
temp=$(nvidia-smi --query-gpu=temperature.gpu --format=csv,noheader,nounits | awk '{printf "%.0f", (($1 * 1.8) + 32)}')

echo ''$vram'GB '$temp'°F'

