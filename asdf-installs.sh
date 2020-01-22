#!/bin/sh

asdf plugin-add python

# TODO make sure you install latest version
# also you might want .tool-versions to point to the system install of python...
for pyv in 2.1.3  2.7.16  3.6.9  3.7.4  3.9-dev  pypy3.6-7.1.1; do
  asdf install python $pyv
done

asdf plugin-add nodejs
echo "You may need to add some GPG keys for the asdf nodejs plugin"
echo "See https://github.com/asdf-vm/asdf-nodejs"

for nv in 12.14.1; do
  asdf install nodejs $nv
done
