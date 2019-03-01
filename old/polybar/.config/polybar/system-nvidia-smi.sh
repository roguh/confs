#!/bin/sh
if type optirun > /dev/null ; then
  optirun nvidia-smi --loop=15 --query-gpu=temperature.gpu,utilization.gpu,utilization.memory --format=csv,noheader,nounits | sed -ur 's|([[:digit:]]+)[^[:digit:]]*([[:digit:]]+)[^[:digit:]]*([[:digit:]]+)|GPU \2% \3% \1°|g'
else
  nvidia-smi --loop=15 --query-gpu=temperature.gpu,utilization.gpu,utilization.memory --format=csv,noheader,nounits | sed -ur 's|([[:digit:]]+)[^[:digit:]]*([[:digit:]]+)[^[:digit:]]*([[:digit:]]+)|GPU \2% \3% \1°|g'
fi
