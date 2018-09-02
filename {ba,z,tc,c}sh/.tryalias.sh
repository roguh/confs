#!/bin/sh

tryalias() { 
    alias_name="$1"
    found=false
    shift

    # Check each argument
    for f in "$@" ; do
        first_word=$(echo "$f" | awk '{print $1}')
        if type "$first_word" >/dev/null 2>&1 ; then 
            alias "$alias_name"="$f"
            found=true
            break
        fi
    done

    # If not found, alias first argument to last argument
    if ! $found ; then
        last_arg="$f"
        alias "$alias_name"="$last_arg" 
    fi
}
