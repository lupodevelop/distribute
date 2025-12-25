#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
BUILD_EBIN="${ROOT_DIR}/build/dev/erlang/distribute/ebin"
COOKIE="cookie_integ"
TMPDIR="$(mktemp -d)"
A_OUT="${TMPDIR}/a.out"
B_OUT="${TMPDIR}/b.out"
C_OUT="${TMPDIR}/c.out"

cleanup() {
  jobs -p | xargs -r kill || true
  rm -rf "${TMPDIR}"
}
trap cleanup EXIT

cd "${ROOT_DIR}"
gleam build
HOST=$(hostname -s)
NODE_A="app_a@${HOST}"
NODE_B="app_b@${HOST}"
NODE_C="app_c@${HOST}"

echo "Starting node A"
erl -pa "${BUILD_EBIN}" -sname app_a -setcookie "${COOKIE}" -noshell -eval \
"membership_ffi:start(500,3,2,5), io:format(\"node_a ready~n\"), timer:sleep(15000), init:stop()." > "${A_OUT}" 2>&1 &

echo "Starting node B"
erl -pa "${BUILD_EBIN}" -sname app_b -setcookie "${COOKIE}" -noshell -eval \
"membership_ffi:start(500,3,2,5), net_adm:ping(list_to_atom(\"app_a@${HOST}\")), io:format(\"node_b ready~n\"), timer:sleep(15000), init:stop()." > "${B_OUT}" 2>&1 &

echo "Starting node C"
erl -pa "${BUILD_EBIN}" -sname app_c -setcookie "${COOKIE}" -noshell -eval \
"membership_ffi:start(500,3,2,5), net_adm:ping(list_to_atom(\"app_a@${HOST}\")), io:format(\"node_c ready~n\"), timer:sleep(15000), init:stop()." > "${C_OUT}" 2>&1 &

echo "Waiting for gossip convergence (10s)"
sleep 10

HOST=$(hostname -s)
NODE_A="app_a@${HOST}"
NODE_B="app_b@${HOST}"
NODE_C="app_c@${HOST}"
echo "Querying membership on each node"
erl -noshell -sname ctrl -setcookie "${COOKIE}" -eval \
"io:format('A:~n~p~n', [rpc:call(list_to_atom(\"${NODE_A}\"), membership_ffi, members_with_status, [])]), io:format('B:~n~p~n', [rpc:call(list_to_atom(\"${NODE_B}\"), membership_ffi, members_with_status, [])]), io:format('C:~n~p~n', [rpc:call(list_to_atom(\"${NODE_C}\"), membership_ffi, members_with_status, [])]), init:stop()." || true

echo "Metrics (from node A):"
erl -noshell -sname ctrl2 -setcookie "${COOKIE}" -eval \
"io:format('metrics: ~p~n', [rpc:call(list_to_atom(\"${NODE_A}\"), membership_ffi, metrics, [])]), init:stop()." || true

echo "Integration script finished; logs in ${TMPDIR}"
echo "Node A log:"; cat "${A_OUT}"
echo "Node B log:"; cat "${B_OUT}"
echo "Node C log:"; cat "${C_OUT}"

exit 0
