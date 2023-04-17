#!/bin/bash

# delete extraneous files from destination
ROOT=$HOME/sync/technical/backups-$(lsb_release --short --id)-$(hostname)

echo $ROOT
