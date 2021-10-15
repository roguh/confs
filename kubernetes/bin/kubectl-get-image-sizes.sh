#!/bin/sh
kubectl get nodes -o json | jq '.items[].status.images[] | .names[1], .sizeBytes / 1E6'
