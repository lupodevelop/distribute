#!/usr/bin/env bash
set -euo pipefail

# Integration test (robust):
# - builds the project
# - starts node A in background registering a receiver
# - starts node B which connects and sends a message
# - retries ping/connect with timeout
# - captures logs and ensures cleanup on exit

ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
APP_DIR="${ROOT_DIR}/examples/two_nodes_app"
TMPDIR="$(mktemp -d)"
A_OUT="${TMPDIR}/node_a.out"
B_OUT="${TMPDIR}/node_b.out"
PID_A=""

cleanup() {
  [ -n "${PID_A}" ] && kill "${PID_A}" 2>/dev/null || true
  rm -rf "${TMPDIR}" 2>/dev/null || true
}
trap cleanup EXIT

echo "[integration] tmpdir=${TMPDIR}"

echo "[integration] Building example app..."
cd "${APP_DIR}"
gleam build

echo "[integration] Starting node A (background)..."
# Run node A using gleam run from the app directory
gleam run -m examples_node_a > "${A_OUT}" 2>&1 &
PID_A=$!

echo "[integration] Node A pid: ${PID_A}"

# Wait for node A to print 'ready' (with timeout)
timeout=6
echo "[integration] waiting up to ${timeout}s for node A to be ready..."
i=0
while [ $i -lt $timeout ]; do
  if grep -q "ready" "${A_OUT}" 2>/dev/null; then
    echo "[integration] node A ready"
    break
  fi
  sleep 1
  i=$((i + 1))
done
if [ $i -ge $timeout ]; then
  echo "[integration][error] node A did not become ready in ${timeout}s"
  echo "--- node A log ---"
  cat "${A_OUT}" || true
  exit 3
fi

echo "[integration] Running node B to send message..."
gleam run -m examples_node_b > "${B_OUT}" 2>&1 || true

echo "[integration] waiting briefly for message delivery"
sleep 1

echo "[integration] Shutting down node A"
kill ${PID_A} || true
wait ${PID_A} 2>/dev/null || true

echo "[integration] Node A output:" 
cat "${A_OUT}" || true
echo "[integration] Node B output:" 
cat "${B_OUT}" || true

if grep -q "calc got" "${A_OUT}" 2>/dev/null; then
  echo "[integration] Integration test passed: message received by node A"
  exit 0
else
  echo "[integration][error] Integration test failed: message not received"
  exit 2
fi
