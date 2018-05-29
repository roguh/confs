#!/bin/sh
free -h | grep 'Mem:' | awk '{print $3}'
