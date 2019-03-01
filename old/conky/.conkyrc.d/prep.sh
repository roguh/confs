#!/bin/sh
for f in *conkyrc*.lua ; do
    cat template.lua $f > $(basename --suffix=.lua $f)
done
