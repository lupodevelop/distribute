#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
EXAMPLE_SCRIPT="$ROOT_DIR/examples/two_nodes/swim_integration.sh"

if [ ! -x "$EXAMPLE_SCRIPT" ]; then
  echo "Making example script executable"
  chmod +x "$EXAMPLE_SCRIPT"
fi

echo "Building project..."
(cd "$ROOT_DIR" && gleam build)

echo "Running SWIM integration example (will run ~10s)..."
OUTPUT="$($EXAMPLE_SCRIPT)"

printf "%s\n" "$OUTPUT"

# Expect at least one 'alive' entry in output
if printf "%s" "$OUTPUT" | grep -q '"alive"'; then
  echo "Integration check: OK (found 'alive' in output)"
  exit 0
else
  echo "Integration check: FAIL (no 'alive' entries found)"
  exit 2
fi
