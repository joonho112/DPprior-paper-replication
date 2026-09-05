#!/bin/sh
# Author: JoonHo Lee (jlee296@ua.edu)
# Launch the isolated rebuild
#
# Resolve this wrapper's location so it works from another directory, then
# pass all arguments to the Python implementation. Keep the wrapper small; the
# test sequence is documented in cold_start.py.
#
# Run: sh verification/cold_start.sh --execute

# Stop on an unset variable or failed command.
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

# Replace the wrapper process so Python's exit status reaches the caller
# unchanged.
exec python3 "$script_dir/cold_start.py" "$@"
