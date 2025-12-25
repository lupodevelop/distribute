#!/usr/bin/env bash
set -euo pipefail

# Wrapper: run the repository-level integration test from the example project
ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
SCRIPT="${ROOT_DIR}/examples/two_nodes/integration_test.sh"

if [ ! -f "${SCRIPT}" ]; then
  echo "Integration script not found at ${SCRIPT}" >&2
  exit 1
fi

echo "Running integration test using repository library build..."
bash "${SCRIPT}"
