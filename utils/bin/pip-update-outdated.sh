#!/bin/bash
PKGS=.outdated-pip-packages

pip3 list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1 > $PKGS

if [ $(wc -l $PKGS | cut -d' ' -f1) -eq 0 ]; then
  echo pip packages fully up-to-date
  exit 0
fi

echo Updating the following packages:
cat $PKGS

for package in $(cat $PKGS); do
  pip install --upgrade $package
done

