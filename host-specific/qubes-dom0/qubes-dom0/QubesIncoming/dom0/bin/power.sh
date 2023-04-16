#!/bin/sh
cat /sys/class/power_supply/BAT*/power_now | awk '{printf $1/10^6 " W\n"}'
cat /sys/class/power_supply/BAT*/energy_now | awk '{printf $1/10^4 " mWh\n"}'
