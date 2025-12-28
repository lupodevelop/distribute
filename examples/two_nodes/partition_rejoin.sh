#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
APP_DIR="${ROOT_DIR}/examples/two_nodes_app"
COOKIE="partition_test_cookie"
TMPDIR="/tmp/partition_test_logs"
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
log_info "  PARTITION & REJOIN INTEGRATION TEST"
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

log_info "Waiting for cluster formation (6s)..."
sleep 6

log_info "=============================================="
log_info "  PHASE 1: Simulate Partition (Stop B)"
log_info "=============================================="

# Pause node B (simulate partition) by stopping its membership service
log_warn "Stopping membership service on Node B..."
erl -noshell -sname ctrl_part -setcookie "${COOKIE}" -eval "
  rpc:call(list_to_atom(\"${NODE_B}\"), membership_ffi, stop, []),
  io:format(\"stopped membership on B~n\"),
  init:stop().
" || true

log_info "Waiting for failure detection (8s)..."
sleep 8

log_info "Checking if Node B is detected as Suspect/Dead by A..."
STATUS_CHECK=$(erl -noshell -sname ctrl_check -setcookie "${COOKIE}" -eval "
  Members = rpc:call(list_to_atom(\"${NODE_A}\"), membership_ffi, members_with_status, []),
  io:format(\"~p\", [Members]),
  init:stop().
" 2>&1) || true

echo "$STATUS_CHECK"

if echo "$STATUS_CHECK" | grep -q "${NODE_B}" && (echo "$STATUS_CHECK" | grep -q "suspect" || echo "$STATUS_CHECK" | grep -q "dead"); then
  log_ok "Node B correctly detected as suspect/dead."
else
  log_warn "Node B status might not be correct (or it's already dead and removed?)"
fi

log_info "=============================================="
log_info "  PHASE 2: Rejoin Node B"
log_info "=============================================="

# Now restart membership on B
log_info "Restarting membership service on Node B..."
erl -noshell -sname ctrl_rejoin -setcookie "${COOKIE}" -eval "
  rpc:call(list_to_atom(\"${NODE_B}\"), membership_ffi, start, [500]),
  io:format(\"restarted membership on B~n\"),
  init:stop().
" || true

log_info "Waiting for gossip convergence (8s)..."
sleep 8

log_info "Checking if Node B is Alive again..."
FINAL_CHECK=$(erl -noshell -sname ctrl_final -setcookie "${COOKIE}" -eval "
  Members = rpc:call(list_to_atom(\"${NODE_A}\"), membership_ffi, members_with_status, []),
  io:format(\"~p\", [Members]),
  init:stop().
" 2>&1) || true

echo "$FINAL_CHECK"

if echo "$FINAL_CHECK" | grep -q "${NODE_B}" && echo "$FINAL_CHECK" | grep -q "alive"; then
  log_ok "SUCCESS: Node B rejoined and is alive."
else
  log_fail "FAILURE: Node B did not rejoin correctly."
  exit 1
fi

echo ""
log_info "Node logs saved in: ${TMPDIR}"
