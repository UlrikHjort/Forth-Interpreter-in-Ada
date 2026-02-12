#!/bin/bash
# Forth interpreter launcher with readline-style history
# This script provides a better interactive experience

FORTH_BIN="$(dirname "$0")/bin/forth"
HISTORY_FILE="$HOME/.forth_history"

# Simple line editing with history
declare -a history
history_index=0

# Load history
if [ -f "$HISTORY_FILE" ]; then
    readarray -t history < "$HISTORY_FILE"
    history_index=${#history[@]}
fi

# Save history on exit
save_history() {
    printf "%s\n" "${history[@]}" > "$HISTORY_FILE"
}
trap save_history EXIT

# Run forth with history support
"$FORTH_BIN"
