#!/bin/sh
TARGET="$1"
if [ -z "$TARGET" ]; then
  echo "USAGE: $0 [target_qube_name]"
  exit 1
fi

set -x
qvm-copy-to-vm "$TARGET" bin .config/{fish,i3,i3status-rust,starship.toml}
echo "Files backed up to $TARGET ~/QubesIncoming"
