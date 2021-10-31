#!/bin/bash
awk '{printf("%s %s %s", $1, $2, $3)}' /proc/loadavg