#!/bin/sh
kubectl get nodes -o json | jq '.items[].status.images[] | .sizeBytes / 1E6, .names[1]' | paste -sd ' \n'
