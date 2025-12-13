#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname "$0")" && pwd)"
source ${SCRIPT_DIR}/test-env.sh
EXT="elc"

cd ${REPO_ROOT}

fd -e ${EXT} --no-ignore -X rm
