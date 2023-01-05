#!/bin/bash
ROOTDIR="$(projectroot.sh)"
ROOT_STATUS_CODE="$?"

if [ "$ROOT_STATUS_CODE" != 0 ]; then
  echo ""
  exit "$ROOT_STATUS_CODE"
else
  PROJECTNAME="$(basename "$ROOTDIR" |
      sed "s/^pfc_\|powerflex_edge_\|powerflex_cloud_\|powerflex_//"
    )"
  echo "$PROJECTNAME"
fi
