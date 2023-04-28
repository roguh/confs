#!/bin/sh
ARGS="$@"
RESULTS_FILE="du-results"

set -x
du --bytes --human-readable "$ARGS" | sort --human-numeric-sort | tee "$RESULTS_FILE"
