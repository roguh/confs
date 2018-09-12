#!/bin/sh
# Poll every 15 seconds
optirun nvidia-smi --loop=15 --query-gpu=temperature.gpu,utilization.gpu,utilization.memory --format=csv,noheader,nounits | sed -ur 's|([[:digit:]]+)[^[:digit:]]*([[:digit:]]+)[^[:digit:]]*([[:digit:]]+)|GPU \2% \3% \1°|g'
