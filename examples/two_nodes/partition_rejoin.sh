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
"membership_ffi:start(500,3,2,5), io:format(\"node_a ready~n\"), timer:sleep(90000), init:stop()." > "${A_OUT}" 2>&1 &

sleep 1

echo "Starting node B"
erl -pa "${BUILD_EBIN}" -sname app_b -setcookie "${COOKIE}" -noshell -eval \
"membership_ffi:start(500,3,2,5), net_adm:ping(list_to_atom(\"${NODE_A}\")), io:format(\"node_b ready~n\"), timer:sleep(90000), init:stop()." > "${B_OUT}" 2>&1 &

sleep 1

echo "Starting node C"
erl -pa "${BUILD_EBIN}" -sname app_c -setcookie "${COOKIE}" -noshell -eval \
"membership_ffi:start(500,3,2,5), net_adm:ping(list_to_atom(\"${NODE_A}\")), io:format(\"node_c ready~n\"), timer:sleep(90000), init:stop()." > "${C_OUT}" 2>&1 &

sleep 6

# Pause node B (simulate partition) by stopping its membership service
echo "Simulating partition by stopping membership on B"
erl -noshell -sname ctrl -setcookie "${COOKIE}" -eval \
"rpc:call(list_to_atom(\"${NODE_B}\"), membership_ffi, stop, []), io:format(\"stopped membership on B~n\"), init:stop()." || true

# Wait for probes to detect partition
sleep 8

# Now restart membership on B
echo "Rejoining node B by starting membership on B"
erl -noshell -sname ctrl2 -setcookie "${COOKIE}" -eval \
"rpc:call(list_to_atom(\"${NODE_B}\"), membership_ffi, start, [500,3,2,5]), io:format(\"restarted membership on B~n\"), init:stop()." || true

sleep 8

# Diagnostic: query node info and connectivity before membership query
echo "Diagnostics from controller before membership query"
erl -noshell -sname ctrl3 -setcookie "${COOKIE}" -eval \
"io:format('Controller node: ~p~n', [node()]), io:format('Controller nodes: ~p~n', [nodes()]), io:format('Pinging all nodes: ~p~n', [ [net_adm:ping(list_to_atom(\"${NODE_A}\")), net_adm:ping(list_to_atom(\"${NODE_B}\")), net_adm:ping(list_to_atom(\"${NODE_C}\"))] ]), init:stop()." || true

# Query membership on each node with diagnostics
erl -noshell -sname ctrl4 -setcookie "${COOKIE}" -eval \
"io:format('A:~n~p~n', [rpc:call(list_to_atom(\"${NODE_A}\"), erlang, node, [])]), io:format('A nodes:~n~p~n', [rpc:call(list_to_atom(\"${NODE_A}\"), erlang, nodes, [])]), io:format('A membership:~n~p~n', [rpc:call(list_to_atom(\"${NODE_A}\"), membership_ffi, members_with_status, [])]),\
io:format('B:~n~p~n', [rpc:call(list_to_atom(\"${NODE_B}\"), erlang, node, [])]), io:format('B nodes:~n~p~n', [rpc:call(list_to_atom(\"${NODE_B}\"), erlang, nodes, [])]), io:format('B membership:~n~p~n', [rpc:call(list_to_atom(\"${NODE_B}\"), membership_ffi, members_with_status, [])]),\
io:format('C:~n~p~n', [rpc:call(list_to_atom(\"${NODE_C}\"), erlang, node, [])]), io:format('C nodes:~n~p~n', [rpc:call(list_to_atom(\"${NODE_C}\"), erlang, nodes, [])]), io:format('C membership:~n~p~n', [rpc:call(list_to_atom(\"${NODE_C}\"), membership_ffi, members_with_status, [])]), init:stop()." || true

# Print logs

echo "Node A log:"; cat "${A_OUT}" || true
echo "Node B log:"; cat "${B_OUT}" || true
echo "Node C log:"; cat "${C_OUT}" || true

exit 0
#!/usr/bin/env bash
