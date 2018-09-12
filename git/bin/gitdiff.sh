#!/bin/sh
C=$(stty size | cut -d' ' -f2)
D=$((C / 2))

echo
echo
printf "%${D}s\n" | tr " " "-"
echo "$1"
printf "%${D}s\n" | tr " " "v"
echo
echo

if command -v icdiff > /dev/null ; then
    # LOL
    icdiff --cols=$((D + D/2 + D/8)) --no-headers --line-numbers "$2" "$5" | grep -v -e '^$' | less
elif command -v sdiff > /dev/null ; then
    sdiff --suppress-common-lines  --ignore-all-space --width=$C "$2" "$5" | less
else
    diff --suppress-common-lines -y "$2" "$5" | less
fi

# Don't die
true

