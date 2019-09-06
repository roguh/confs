#!/bin/sh

asdf plugin-add python

# TODO make sure you install latest version
# also you might want .tool-versions to point to the system install of python...
for pyv in 2.1.3  2.7.16  3.6.9  3.7.4  3.9-dev  pypy3.6-7.1.1
  asdf install python $pyv
done
