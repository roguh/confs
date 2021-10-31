#!/bin/bash
C=$(stty size | cut -d' ' -f2)
D=$((C / 2))

echo
echo
printf "%${D}s\n" | tr " " "-"
echo "$1"
printf "%${D}s\n" | tr " " "v"
echo
echo

if command -v meld > /dev/null ; then
    meld "$2" "$5"
elif command -v sdiff > /dev/null ; then
    sdiff -l --ignore-all-space --width=$C "$2" "$5" | less
else
    diff -y "$2" "$5" | less
fi

# Don't die
true

