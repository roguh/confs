#!/bin/bash
set -x
ANY_ON="$(hueadm group --json 0 | jq .state.any_on)"
if [ "$ANY_ON" = "true" ]; then
  hueadm group 0 off
else
  hueadm group 0 on
fi
