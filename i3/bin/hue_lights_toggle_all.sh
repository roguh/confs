#!/bin/bash
set -x
ARGS="-c $HOME/.hueadm.json"
ANY_ON="$(hueadm $ARGS group --json 0 | jq .state.any_on)"
if [ "$ANY_ON" = "true" ]; then
  hueadm $ARGS group 0 off
else
  hueadm $ARGS group 0 on
fi
