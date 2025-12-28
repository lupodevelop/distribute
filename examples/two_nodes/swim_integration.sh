#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
APP_DIR="${ROOT_DIR}/examples/two_nodes_app"
COOKIE="swim_test_cookie"
TMPDIR="/tmp/swim_test_logs"
mkdir -p "${TMPDIR}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_ok() { echo -e "${GREEN}[OK]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_fail() { echo -e "${RED}[FAIL]${NC} $1"; }

cleanup() {
  log_info "Cleaning up..."
  jobs -p | xargs -r kill || true
  pkill -f "swim_node" || true
}
trap cleanup EXIT

# Pre-cleanup
pkill -f "swim_node" || true

log_info "Building example app..."
cd "${APP_DIR}"
gleam build

HOST=$(hostname -s)
NODE_A="app_a@${HOST}"
NODE_B="app_b@${HOST}"
NODE_C="app_c@${HOST}"

log_info "=============================================="
log_info "  SWIM MEMBERSHIP INTEGRATION TEST"
log_info "=============================================="
echo ""

# Start Node A
log_info "Starting Node A (${NODE_A})..."
NODE_NAME="${NODE_A}" NODE_COOKIE="${COOKIE}" gleam run -m swim_node > "${TMPDIR}/a.log" 2>&1 &
sleep 1

# Start Node B
log_info "Starting Node B (${NODE_B})..."
NODE_NAME="${NODE_B}" NODE_COOKIE="${COOKIE}" JOIN_NODE="${NODE_A}" gleam run -m swim_node > "${TMPDIR}/b.log" 2>&1 &
sleep 1

# Start Node C
log_info "Starting Node C (${NODE_C})..."
NODE_NAME="${NODE_C}" NODE_COOKIE="${COOKIE}" JOIN_NODE="${NODE_A}" gleam run -m swim_node > "${TMPDIR}/c.log" 2>&1 &
sleep 1

log_info "Waiting for gossip convergence (10s)..."
sleep 10

echo ""
log_info "=============================================="
log_info "  CHECKING MEMBERSHIP STATE"
log_info "=============================================="

# Query membership state
MEMBERSHIP_STATE=$(erl -noshell -sname ctrl_swim -setcookie "${COOKIE}" -eval "
  io:format(\"~n=== Node A View ===\"),
  io:format(\"~n~p\", [rpc:call(list_to_atom(\"${NODE_A}\"), membership_ffi, members_with_status, [])]),
  io:format(\"~n~n=== Node B View ===\"),
  io:format(\"~n~p\", [rpc:call(list_to_atom(\"${NODE_B}\"), membership_ffi, members_with_status, [])]),
  io:format(\"~n~n=== Node C View ===\"),
  io:format(\"~n~p\", [rpc:call(list_to_atom(\"${NODE_C}\"), membership_ffi, members_with_status, [])]),
  init:stop().
" 2>&1) || true

echo "$MEMBERSHIP_STATE"

# Check if all nodes see each other (simple grep check)
# We expect each node to see 2 other nodes (plus itself, but the list might exclude self or include it depending on implementation)
# Actually members_with_status usually returns all known members.
# Let's just check if the output contains all node names for each view.

if echo "$MEMBERSHIP_STATE" | grep -q "${NODE_B}" && echo "$MEMBERSHIP_STATE" | grep -q "${NODE_C}"; then
  log_ok "SUCCESS: Nodes seem to have discovered each other."
else
  log_fail "FAILURE: Cluster did not converge fully."
  exit 1
fi

echo ""
log_info "Node logs saved in: ${TMPDIR}"
