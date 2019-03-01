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
    icdiff --cols=$((2 * D)) --no-headers --line-numbers "$2" "$5" | grep -v -e '^$' | less
elif command -v sdiff > /dev/null ; then
    sdiff -l --suppress-common-lines  --ignore-all-space --width=$C "$2" "$5" | less
else
    diff --suppress-common-lines -y "$2" "$5" | less
fi

# Don't die
true

