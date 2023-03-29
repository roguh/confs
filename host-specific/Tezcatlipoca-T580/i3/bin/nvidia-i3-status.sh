#!/bin/bash
set -o pipefail

if ! nvidia-smi --query-gpu=memory.used --format=csv > /dev/zero ; then
  echo '🚫nv'
  exit 1
fi

set -x
vrams=$(nvidia-smi --query-gpu=memory.used --format=csv,noheader,nounits | awk '{printf "%.2fGB ", $1 / 1024}')
temps=$(nvidia-smi --query-gpu=temperature.gpu --format=csv,noheader,nounits | awk '{printf "%.0f°F ", (($1 * 1.8) + 32)}')

echo "$vrams $temps"

