#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname "$0")" && pwd)"
source ${SCRIPT_DIR}/test-env.sh

# Safety checks before removing database
if [[ -z "${DB_PATH:-}" ]]; then
  echo "Error: DB_PATH is not set" >&2
  exit 1
fi

if [[ ! "${DB_PATH}" =~ (test|tmp) ]]; then
  echo "Error: DB_PATH must contain /test/ or /tmp/ for safety: ${DB_PATH}" >&2
  exit 1
fi

if [[ -e "${DB_PATH}" ]]; then
  rm -rf "${DB_PATH}"
  echo "Removed existing database: ${DB_PATH}"
fi

emacs -Q --batch \
  --eval "(setq org-tasktree-database-location \"${DB_PATH}\")" \
  --eval "(setq org-tasktree-query-dir \"${QUERY_DIR}\")" \
  --eval "(setq repo-root \"${REPO_ROOT}\")" \
  -l "${SCRIPT_DIR}/db-init.el"
