#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
SRC_DIR="${ROOT_DIR}/src"
TMP_DIR="${SRC_DIR}/examples_tmp"

echo "Temporarily copying examples into ${TMP_DIR} to validate build..."
rm -rf "${TMP_DIR}"
mkdir -p "${TMP_DIR}"

cp "${ROOT_DIR}/examples/two_nodes/"*.gleam "${TMP_DIR}/"

echo "Running gleam build (including temporary examples)..."
cd "${ROOT_DIR}"
set +e
gleam build
BUILD_EXIT=$?
set -e

echo "Cleaning up temporary examples..."
rm -rf "${TMP_DIR}"

if [ ${BUILD_EXIT} -ne 0 ]; then
  echo "Build failed with exit code ${BUILD_EXIT}"
  exit ${BUILD_EXIT}
else
  echo "Build succeeded (examples compile cleanly)."
fi

exit 0
