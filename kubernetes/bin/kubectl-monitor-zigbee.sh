#!/bin/bash
set -x
watch -n1 'kubectl get pods -o wide | grep zigbee'
