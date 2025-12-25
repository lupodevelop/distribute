#!/usr/bin/env bash
set -euxo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
BUILD_EBIN="${ROOT_DIR}/_build/default/lib/distribute/ebin"
COOKIE="cookie_integ"

cd "${ROOT_DIR}"

# Build the project
gleam build

echo "Built library into _build/default/lib/distribute/ebin"

echo "Examples have been moved to 'examples/two_nodes/'. They are not compiled as part of the main library build to keep src/ clean."
echo "To run the examples you can:"
echo "  1) Compile the library (done)."
echo "  2) Start two Erlang nodes that load the library beams and call the example functions manually."
echo "     Example commands (run in separate terminals):"
echo "       erl -pa ${BUILD_EBIN} -sname app_a -setcookie ${COOKIE} -noshell -eval \"io:format('Start node A and call example manually\n'), init:stop().\""
echo "       erl -pa ${BUILD_EBIN} -sname app_b -setcookie ${COOKIE} -noshell -eval \"io:format('Start node B and call example manually\n'), init:stop().\""

echo "If you want, I can convert the examples into a standalone example project that builds and runs automatically."

exit 0
