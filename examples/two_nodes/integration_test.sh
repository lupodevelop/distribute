#!/usr/bin/env bash
set -euo pipefail

# Integration test (robust):
# - builds the project
# - starts node A in background registering a receiver
# - starts node B which connects and sends a message
# - retries ping/connect with timeout
# - captures logs and ensures cleanup on exit

ROOT_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
BUILD_EBIN="${ROOT_DIR}/build/dev/erlang/distribute/ebin"
COOKIE="cookie_integ"
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

echo "[integration] Building library..."
cd "${ROOT_DIR}"
gleam build

echo "[integration] Starting node A (background)..."
# Start node A which registers a receiver and waits (timeout sec)
erl -pa "${BUILD_EBIN}" -sname app_a -setcookie "${COOKIE}" -noshell -eval \
"membership_ffi:start(500), P = spawn(fun() -> receive X -> io:format(\"calc got: ~p~n\", [X]) end end), global:register_name(calculator, P), io:format(\"node_a ready~n\"), timer:sleep(10000), init:stop()." > "${A_OUT}" 2>&1 &
PID_A=$!

echo "[integration] Node A pid: ${PID_A}"

# Wait for node A to print 'node_a ready' (with timeout)
timeout=6
echo "[integration] waiting up to ${timeout}s for node A to be ready..."
i=0
while [ $i -lt $timeout ]; do
  if grep -q "node_a ready" "${A_OUT}" 2>/dev/null; then
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

## Create a small Erlang helper module for node B to avoid shell quoting issues
cat > "${TMPDIR}/node_b.erl" <<'EOL'
-module(node_b).
-export([start/0]).

start() ->
    io:format("node_b connecting~n"),
    membership_ffi:start(500),
    {ok, Host} = inet:gethostname(),
    HostStr = lists:flatten(Host),
    NodeAtom = list_to_atom("app_a@" ++ HostStr),
    io:format("pinging ~p~n", [NodeAtom]),
    case do_ping(NodeAtom, 5) of
      pong -> io:format("ping ok~n");
      {error, timeout} -> erlang:error(ping_failed)
    end,
    io:format("node_b connected~n"),
    timer:sleep(200),
    io:format("node_b sending~n"),
    messaging:send_global("calculator", "Hello from node B!"),
    io:format("node_b sent~n"),
    ok.

do_ping(Node, 0) ->
  {error, timeout};
do_ping(Node, N) ->
  case net_adm:ping(Node) of
    pong -> pong;
    pang -> timer:sleep(200), do_ping(Node, N-1)
  end.
EOL

erlc -o "${TMPDIR}" "${TMPDIR}/node_b.erl"

erl -pa "${BUILD_EBIN}" -pa "${TMPDIR}" -sname app_b -setcookie "${COOKIE}" -noshell -eval "node_b:start(), init:stop()." > "${B_OUT}" 2>&1 || true

echo "[integration] waiting briefly for message delivery"
sleep 1

echo "[integration] Shutting down node A"
kill ${PID_A} || true

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
#!/usr/bin/env bash
