#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
APP_DIR="${ROOT_DIR}/examples/two_nodes_app"
COOKIE="raft_test_cookie"
TMPDIR="/tmp/raft_test_logs"
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
  # Kill any lingering beam processes for this app
  pkill -f "raft_node" || true
  # rm -rf "${TMPDIR}"
}
trap cleanup EXIT

# Pre-cleanup
pkill -f "raft_node" || true

log_info "Building example app..."
cd "${APP_DIR}"
gleam build

HOST=$(hostname -s)
NODE_A="app_a@${HOST}"
NODE_B="app_b@${HOST}"
NODE_C="app_c@${HOST}"

# PIDs for each node
PID_A=""
PID_B=""
PID_C=""

log_info "=============================================="
log_info "  RAFT LEADER FAILOVER TEST"
log_info "=============================================="
echo ""

# Start Node A
log_info "Starting Node A (${NODE_A})..."
NODE_NAME="${NODE_A}" NODE_COOKIE="${COOKIE}" gleam run -m raft_node > "${TMPDIR}/a.log" 2>&1 &
PID_A=$!
sleep 1

# Start Node B
log_info "Starting Node B (${NODE_B})..."
NODE_NAME="${NODE_B}" NODE_COOKIE="${COOKIE}" JOIN_NODE="${NODE_A}" gleam run -m raft_node > "${TMPDIR}/b.log" 2>&1 &
PID_B=$!
sleep 1

# Start Node C
log_info "Starting Node C (${NODE_C})..."
NODE_NAME="${NODE_C}" NODE_COOKIE="${COOKIE}" JOIN_NODE="${NODE_A}" gleam run -m raft_node > "${TMPDIR}/c.log" 2>&1 &
PID_C=$!
sleep 1

log_info "Waiting for cluster formation (5s)..."
sleep 5

echo ""
log_info "=============================================="
log_info "  PHASE 1: Check initial cluster state"
log_info "=============================================="

# Query cluster state
CLUSTER_STATE=$(erl -noshell -sname ctrl1 -setcookie "${COOKIE}" -eval "
  io:format(\"~n=== Cluster Membership ===\"),
  io:format(\"~nNode A members: ~p\", [rpc:call(list_to_atom(\"${NODE_A}\"), membership_ffi, members_with_status, [])]),
  io:format(\"~nNode B members: ~p\", [rpc:call(list_to_atom(\"${NODE_B}\"), membership_ffi, members_with_status, [])]),
  io:format(\"~nNode C members: ~p\", [rpc:call(list_to_atom(\"${NODE_C}\"), membership_ffi, members_with_status, [])]),
  io:format(\"~n~n=== Connected Nodes ===\"),
  io:format(\"~nNode A connected to: ~p\", [rpc:call(list_to_atom(\"${NODE_A}\"), erlang, nodes, [])]),
  io:format(\"~nNode B connected to: ~p\", [rpc:call(list_to_atom(\"${NODE_B}\"), erlang, nodes, [])]),
  io:format(\"~nNode C connected to: ~p\", [rpc:call(list_to_atom(\"${NODE_C}\"), erlang, nodes, [])]),
  io:format(\"~n~n=== Raft State ===\"),
  io:format(\"~nNode A raft state: ~p\", [rpc:call(list_to_atom(\"${NODE_A}\"), raft_ffi, get_state, [])]),
  io:format(\"~nNode B raft state: ~p\", [rpc:call(list_to_atom(\"${NODE_B}\"), raft_ffi, get_state, [])]),
  io:format(\"~nNode C raft state: ~p\", [rpc:call(list_to_atom(\"${NODE_C}\"), raft_ffi, get_state, [])]),
  io:format(\"~n\"),
  init:stop().
" 2>&1) || true

echo "$CLUSTER_STATE"

echo ""
log_info "=============================================="
log_info "  PHASE 2: Trigger leader election on Node A"
log_info "=============================================="

# Trigger election from Node A
erl -noshell -sname ctrl2 -setcookie "${COOKIE}" -eval "
  io:format(\"~nTriggering election from Node A...~n\"),
  rpc:call(list_to_atom(\"${NODE_A}\"), raft_ffi, start_election, []),
  init:stop().
" 2>&1 || true

log_info "Waiting for election to complete (3s)..."
sleep 3

# Check who became leader
LEADER_CHECK=$(erl -noshell -sname ctrl3 -setcookie "${COOKIE}" -eval "
  StateA = rpc:call(list_to_atom(\"${NODE_A}\"), raft_ffi, get_state, []),
  StateB = rpc:call(list_to_atom(\"${NODE_B}\"), raft_ffi, get_state, []),
  StateC = rpc:call(list_to_atom(\"${NODE_C}\"), raft_ffi, get_state, []),
  io:format(\"~n=== Raft State After Election ===\"),
  io:format(\"~nNode A: ~p\", [StateA]),
  io:format(\"~nNode B: ~p\", [StateB]),
  io:format(\"~nNode C: ~p\", [StateC]),
  %% Find the leader
  LeaderA = case StateA of #{leader := L1} -> L1; _ -> none end,
  LeaderB = case StateB of #{leader := L2} -> L2; _ -> none end,
  LeaderC = case StateC of #{leader := L3} -> L3; _ -> none end,
  
  %% Simple consensus check
  if 
     LeaderA =/= none -> io:format(\"~nLEADER: ~s\", [LeaderA]);
     LeaderB =/= none -> io:format(\"~nLEADER: ~s\", [LeaderB]);
     LeaderC =/= none -> io:format(\"~nLEADER: ~s\", [LeaderC]);
     true -> io:format(\"~nLEADER: none\")
  end,
  init:stop().
" 2>&1) || true

echo "$LEADER_CHECK"

# Determine current leader
CURRENT_LEADER=$(echo "$LEADER_CHECK" | grep "LEADER:" | head -n 1 | awk '{print $2}') || true

echo ""
log_info "Current leader: ${CURRENT_LEADER}"

echo ""
log_info "=============================================="
log_info "  PHASE 3: Kill the leader node"
log_info "=============================================="

KILLED_NODE=""
REMAINING_NODES=""

# Kill the leader
if [[ "${CURRENT_LEADER}" == *"app_a"* ]]; then
  log_warn "Killing Node A (PID ${PID_A})"
  kill ${PID_A} || true
  KILLED_NODE="A"
  REMAINING_NODES="B and C"
elif [[ "${CURRENT_LEADER}" == *"app_b"* ]]; then
  log_warn "Killing Node B (PID ${PID_B})"
  kill ${PID_B} || true
  KILLED_NODE="B"
  REMAINING_NODES="A and C"
elif [[ "${CURRENT_LEADER}" == *"app_c"* ]]; then
  log_warn "Killing Node C (PID ${PID_C})"
  kill ${PID_C} || true
  KILLED_NODE="C"
  REMAINING_NODES="A and B"
else
  log_warn "No leader found or unknown leader. Killing Node A by default."
  kill ${PID_A} || true
  KILLED_NODE="A"
  REMAINING_NODES="B and C"
fi

log_info "Leader node ${KILLED_NODE} killed. Remaining: ${REMAINING_NODES}"
log_info "Waiting for failure detection and re-election (8s)..."
sleep 8

echo ""
log_info "=============================================="
log_info "  PHASE 4: Check new leader election"
log_info "=============================================="

# Trigger new election on remaining nodes
if [[ "${KILLED_NODE}" != "B" ]]; then
  erl -noshell -sname ctrl5 -setcookie "${COOKIE}" -eval "
    io:format(\"~nTriggering election from Node B...~n\"),
    rpc:call(list_to_atom(\"${NODE_B}\"), raft_ffi, start_election, []),
    init:stop().
  " 2>&1 || true
else
  erl -noshell -sname ctrl5 -setcookie "${COOKIE}" -eval "
    io:format(\"~nTriggering election from Node A...~n\"),
    rpc:call(list_to_atom(\"${NODE_A}\"), raft_ffi, start_election, []),
    init:stop().
  " 2>&1 || true
fi

sleep 3

# Query remaining nodes
FINAL_STATE=$(erl -noshell -sname ctrl6 -setcookie "${COOKIE}" -eval "
  StateA = rpc:call(list_to_atom(\"${NODE_A}\"), raft_ffi, get_state, []),
  StateB = rpc:call(list_to_atom(\"${NODE_B}\"), raft_ffi, get_state, []),
  StateC = rpc:call(list_to_atom(\"${NODE_C}\"), raft_ffi, get_state, []),
  io:format(\"~n=== Final Raft State ===\"),
  io:format(\"~nNode A: ~p\", [StateA]),
  io:format(\"~nNode B: ~p\", [StateB]),
  io:format(\"~nNode C: ~p\", [StateC]),
  
  LeaderA = case StateA of #{leader := L1} -> L1; _ -> none end,
  LeaderB = case StateB of #{leader := L2} -> L2; _ -> none end,
  LeaderC = case StateC of #{leader := L3} -> L3; _ -> none end,
  
  if 
     LeaderA =/= none, LeaderA =/= {badrpc, nodedown} -> io:format(\"~nNEW_LEADER: ~s\", [LeaderA]);
     LeaderB =/= none, LeaderB =/= {badrpc, nodedown} -> io:format(\"~nNEW_LEADER: ~s\", [LeaderB]);
     LeaderC =/= none, LeaderC =/= {badrpc, nodedown} -> io:format(\"~nNEW_LEADER: ~s\", [LeaderC]);
     true -> io:format(\"~nNEW_LEADER: none\")
  end,
  init:stop().
" 2>&1) || true

echo "$FINAL_STATE"

# Check if new leader was elected
NEW_LEADER=$(echo "$FINAL_STATE" | grep "NEW_LEADER:" | head -n 1 | awk '{print $2}') || true

echo ""
log_info "=============================================="
log_info "  TEST RESULTS"
log_info "=============================================="
echo ""
echo "  Initial leader:    ${CURRENT_LEADER}"
echo "  Killed node:       Node ${KILLED_NODE}"
echo "  New leader:        ${NEW_LEADER}"
echo ""

EXIT_CODE=0
if [[ "${NEW_LEADER}" != "none" ]] && [[ "${NEW_LEADER}" != "${CURRENT_LEADER}" ]] && [[ "${NEW_LEADER}" != "" ]]; then
  log_ok "SUCCESS: New leader elected (${NEW_LEADER})"
  EXIT_CODE=0
elif [[ "${NEW_LEADER}" == "${CURRENT_LEADER}" ]]; then
  log_fail "FAILURE: Leader did not change (still ${CURRENT_LEADER})"
  EXIT_CODE=1
else
  log_fail "FAILURE: No leader elected"
  EXIT_CODE=1
fi

echo ""
log_info "Node logs saved in: ${TMPDIR}"
echo ""
echo "=== Node A Log ==="
cat "${TMPDIR}/a.log" 2>/dev/null || echo "(no log)"
echo ""
echo "=== Node B Log ==="
cat "${TMPDIR}/b.log" 2>/dev/null || echo "(no log)"
echo ""
echo "=== Node C Log ==="
cat "${TMPDIR}/c.log" 2>/dev/null || echo "(no log)"

exit ${EXIT_CODE}
