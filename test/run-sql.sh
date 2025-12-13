#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname "$0")" && pwd)"
source ${SCRIPT_DIR}/test-env.sh

SQL_FILE="${1:-}"
if [[ -z "${SQL_FILE}" ]]; then
  echo "Usage: run-sql.sh path/to/file.sql" >&2
  exit 1
fi

sqlite3 "${DB_PATH}" < "${SQL_FILE}"
