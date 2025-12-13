#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname "$0")" && pwd)"
source ${SCRIPT_DIR}/test-env.sh

emacs -Q --batch \
  --eval "(setq org-tasktree-database-location \"${DB_PATH}\")" \
  --eval "(setq org-tasktree-query-dir \"${QUERY_DIR}\")" \
  --eval "(setq repo-root \"${REPO_ROOT}\")" \
  -l "${SCRIPT_DIR}/db-init.el"
