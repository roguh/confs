#!/bin/bash
lscpu | grep 'CPU MHz' | awk '{printf("%.1fGHz", $3/1000)}'