#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname "$0")" && pwd)"
source "${SCRIPT_DIR}/test-env.sh"

HELPER_FILE="${SCRIPT_DIR}/org-tasktree-test-helper.el"

if [[ $# -gt 0 ]]; then
  TEST_FILES=()
  for arg in "$@"; do
    if [[ -f "${arg}" ]]; then
      TEST_FILES+=("${arg}")
    elif [[ -f "${SCRIPT_DIR}/${arg}" ]]; then
      TEST_FILES+=("${SCRIPT_DIR}/${arg}")
    else
      echo "File not found: ${arg}" >&2
      exit 1
    fi
  done
  mapfile -t TEST_FILES < <(printf '%s\n' "${TEST_FILES[@]}" | sort)
else
  mapfile -t TEST_FILES < <(fd --full-path -e el --search-path "${SCRIPT_DIR}" '.*-ert\.el$' | sort)
fi

EMACS_ARGS=(
  "-Q"
  "--batch"
  "--eval" "(setq org-tasktree-database-location \"${DB_PATH}\")"
  "--eval" "(setq org-tasktree-query-dir \"${QUERY_DIR}\")"
  "--eval" "(setq repo-root \"${REPO_ROOT}\")"
  "--eval" "(add-to-list 'load-path \"${REPO_ROOT}\")"
  "--eval" "(add-to-list 'load-path \"${SCRIPT_DIR}\")"
)

if [[ -f "${HELPER_FILE}" ]]; then
  EMACS_ARGS+=("-l" "${HELPER_FILE}")
fi

for file in "${TEST_FILES[@]}"; do
  EMACS_ARGS+=("-l" "${file}")
done

EMACS_ARGS+=(
  "--eval" "(require 'ert)"
  "--eval" "(ert-run-tests-batch-and-exit)"
)

emacs "${EMACS_ARGS[@]}"
