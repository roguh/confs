#!/bin/sh
free | grep 'Mem:' | awk '{printf("%.1fGi", ($3 + $5) / 1000000)}'
