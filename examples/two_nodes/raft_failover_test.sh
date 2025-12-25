#!/usr/bin/env bash
# =============================================================================
# Raft Leader Failover Integration Test
# =============================================================================
# This test:
# 1. Starts 3 nodes (A, B, C) with Raft-lite election service
# 2. Waits for leader election
# 3. Identifies the leader
# 4. Kills the leader node
# 5. Verifies a new leader is elected among remaining nodes
# =============================================================================

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
BUILD_EBIN="${ROOT_DIR}/build/dev/erlang/distribute/ebin"
COOKIE="raft_test_cookie"
TMPDIR="$(mktemp -d)"

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
  jobs -p 2>/dev/null | xargs -r kill 2>/dev/null || true
  rm -rf "${TMPDIR}"
}
trap cleanup EXIT

cd "${ROOT_DIR}"
log_info "Building project..."
gleam build

HOST=$(hostname -s)
NODE_A="app_a@${HOST}"
NODE_B="app_b@${HOST}"
NODE_C="app_c@${HOST}"

# PIDs for each node
PID_A=""
PID_B=""
PID_C=""

start_node() {
  local name=$1
  local logfile=$2
  local others=$3
  
  erl -pa "${BUILD_EBIN}" -sname "${name}" -setcookie "${COOKIE}" -noshell -eval "
    %% Start membership service
    membership_ffi:start(300, 3, 2, 5),
    %% Start raft service
    raft_ffi:start(),
    %% Connect to other nodes
    ${others}
    %% Report ready
    io:format(\"[~s] Node started, connecting to cluster~n\", [node()]),
    %% Keep alive
    receive stop -> ok after 60000 -> ok end,
    init:stop().
  " > "${logfile}" 2>&1 &
  echo $!
}

log_info "=============================================="
log_info "  RAFT LEADER FAILOVER TEST"
log_info "=============================================="
echo ""

# Start Node A (first node, no connections)
log_info "Starting Node A (${NODE_A})..."
PID_A=$(start_node "app_a" "${TMPDIR}/a.log" "ok,")
sleep 1

# Start Node B (connects to A)
log_info "Starting Node B (${NODE_B})..."
PID_B=$(start_node "app_b" "${TMPDIR}/b.log" "net_adm:ping(list_to_atom(\"${NODE_A}\")),")
sleep 1

# Start Node C (connects to A)
log_info "Starting Node C (${NODE_C})..."
PID_C=$(start_node "app_c" "${TMPDIR}/c.log" "net_adm:ping(list_to_atom(\"${NODE_A}\")),")
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
  io:format(\"~n~n=== Leader from each node's perspective ===\"),
  io:format(\"~nNode A sees leader: ~p\", [LeaderA]),
  io:format(\"~nNode B sees leader: ~p\", [LeaderB]),
  io:format(\"~nNode C sees leader: ~p\", [LeaderC]),
  %% Check if A is leader
  RoleA = case StateA of #{role := R1} -> R1; _ -> unknown end,
  io:format(\"~n~nNode A role: ~p\", [RoleA]),
  io:format(\"~n\"),
  init:stop().
" 2>&1) || true

echo "$LEADER_CHECK"

# Determine current leader
CURRENT_LEADER=$(erl -noshell -sname ctrl4 -setcookie "${COOKIE}" -eval "
  State = rpc:call(list_to_atom(\"${NODE_A}\"), raft_ffi, get_state, []),
  Leader = case State of #{leader := L} -> L; _ -> \"none\" end,
  io:format(\"~s\", [Leader]),
  init:stop().
" 2>&1) || true

echo ""
log_info "Current leader: ${CURRENT_LEADER}"

echo ""
log_info "=============================================="
log_info "  PHASE 3: Kill the leader node"
log_info "=============================================="

# Kill the leader
if [[ "${CURRENT_LEADER}" == *"app_a"* ]]; then
  log_warn "Killing Node A (the leader)..."
  kill ${PID_A} 2>/dev/null || true
  KILLED_NODE="A"
  REMAINING_NODES="B and C"
elif [[ "${CURRENT_LEADER}" == *"app_b"* ]]; then
  log_warn "Killing Node B (the leader)..."
  kill ${PID_B} 2>/dev/null || true
  KILLED_NODE="B"
  REMAINING_NODES="A and C"
elif [[ "${CURRENT_LEADER}" == *"app_c"* ]]; then
  log_warn "Killing Node C (the leader)..."
  kill ${PID_C} 2>/dev/null || true
  KILLED_NODE="C"
  REMAINING_NODES="A and B"
else
  log_warn "No clear leader, killing Node A anyway..."
  kill ${PID_A} 2>/dev/null || true
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
fi

sleep 3

# Query remaining nodes
FINAL_STATE=$(erl -noshell -sname ctrl6 -setcookie "${COOKIE}" -eval "
  io:format(\"~n=== Final Cluster State ===\"),
  case rpc:call(list_to_atom(\"${NODE_B}\"), raft_ffi, get_state, []) of
    {badrpc, _} -> io:format(\"~nNode B: DOWN\");
    StateB -> io:format(\"~nNode B raft state: ~p\", [StateB])
  end,
  case rpc:call(list_to_atom(\"${NODE_C}\"), raft_ffi, get_state, []) of
    {badrpc, _} -> io:format(\"~nNode C: DOWN\");
    StateC -> io:format(\"~nNode C raft state: ~p\", [StateC])
  end,
  io:format(\"~n~n=== Membership Status ===\"),
  case rpc:call(list_to_atom(\"${NODE_B}\"), membership_ffi, members_with_status, []) of
    {badrpc, _} -> ok;
    MembersB -> io:format(\"~nNode B members: ~p\", [MembersB])
  end,
  case rpc:call(list_to_atom(\"${NODE_C}\"), membership_ffi, members_with_status, []) of
    {badrpc, _} -> ok;
    MembersC -> io:format(\"~nNode C members: ~p\", [MembersC])
  end,
  io:format(\"~n~n=== Metrics ===\"),
  case rpc:call(list_to_atom(\"${NODE_B}\"), membership_ffi, metrics, []) of
    {badrpc, _} -> ok;
    MetricsB -> io:format(\"~nNode B metrics: ~p\", [MetricsB])
  end,
  io:format(\"~n\"),
  init:stop().
" 2>&1) || true

echo "$FINAL_STATE"

# Check if new leader was elected
NEW_LEADER=$(erl -noshell -sname ctrl7 -setcookie "${COOKIE}" -eval "
  StateB = rpc:call(list_to_atom(\"${NODE_B}\"), raft_ffi, get_state, []),
  Leader = case StateB of #{leader := L} -> L; _ -> \"none\" end,
  io:format(\"~s\", [Leader]),
  init:stop().
" 2>&1) || true

echo ""
log_info "=============================================="
log_info "  TEST RESULTS"
log_info "=============================================="
echo ""
echo "  Initial leader:    ${CURRENT_LEADER}"
echo "  Killed node:       Node ${KILLED_NODE}"
echo "  New leader:        ${NEW_LEADER}"
echo ""

if [[ "${NEW_LEADER}" != "none" ]] && [[ "${NEW_LEADER}" != "${CURRENT_LEADER}" ]] && [[ "${NEW_LEADER}" != "" ]]; then
  log_ok "SUCCESS! New leader elected after failover: ${NEW_LEADER}"
  EXIT_CODE=0
elif [[ "${NEW_LEADER}" == "${CURRENT_LEADER}" ]]; then
  log_warn "Leader unchanged (original leader may not have been killed)"
  EXIT_CODE=1
else
  log_fail "FAILED! No new leader elected"
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
