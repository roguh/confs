#!/bin/bash
set -pipefail

if ! nvidia-smi --query-gpu=memory.used 2>&1 > /dev/zero ; then
  echo No NVIDIA
  exit 1
fi

set -x
vram=$(nvidia-smi --query-gpu=memory.used --format=csv,noheader,nounits | awk '{printf "%.2f", $1 / 1024}')
temp=$(nvidia-smi --query-gpu=temperature.gpu --format=csv,noheader,nounits | awk '{printf "%.0f", (($1 * 1.8) + 32)}')

echo ''$vram'GB '$temp'°F'

