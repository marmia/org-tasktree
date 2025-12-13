#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname "$0")" && pwd)"
source ${SCRIPT_DIR}/test-env.sh

# Insert
#PARENT='(:title "Demo" :node-type "project" :todo-keyword "PROJ" :level 1 :status "OPEN")'
#CHILDREN='((:title "Child A" :node-type "task" :todo-keyword "TODO" :level 2 :status "OPEN"))'

# Update
#PARENT='(:uid "A02C9C52-282F-45BA-81E5-A987625F1335" :title "Demo (Upd2)" :node-type "project" :todo-keyword "PROJ" :level 1 :status "OPEN")'
#CHILDREN='((:uid "17491C15-FE7F-4351-A3C4-3C6633BA7B79" :title "Child A (Upd2)" :node-type "task" :todo-keyword "TODO" :level 2 :status "OPEN"))'

# Tags
PARENT='(:title "Tag Test :Parent" :node-type "project" :todo-keyword "PROJ" :level 1 :status "OPEN" :tags "dev:test")'
CHILDREN='((:title "Tag Test :Child" :node-type "task" :todo-keyword "TODO" :level 2 :status "OPEN" :tags "dev:emacs"))'

# Update Tags
#PARENT='(:uid "44246979-1748-4005-AF22-0BAC4CC77520" :title "Tag Test :Parent" :node-type "project" :todo-keyword "PROJ" :level 1 :status "OPEN" :tags "dev:test:upd")'
#CHILDREN='((:uid "803A6E84-A866-4ECB-9637-23A7105065FD" :title "Tag Test :Child" :node-type "task" :todo-keyword "TODO" :level 2 :status "OPEN" :tags "dev:emacs:upd"))'

# Error Test
#PARENT='(:title "Error Test" :node-type "hello" :todo-keyword "HELLO" :level 1 :status "OPEN" :tags "dev:hello")'

emacs -Q --batch \
  -L "${REPO_ROOT}" \
  --eval "(setq org-tasktree-database-location \"${DB_PATH}\")" \
  --eval "(setq org-tasktree-query-dir \"${QUERY_DIR}\")" \
  --eval "(setq org-tasktree-parent-node '${PARENT})" \
  --eval "(setq org-tasktree-child-nodes '${CHILDREN})" \
  -l "${SCRIPT_DIR}/create-node.el"

echo "create-node: inserted parent/children into ${DB_PATH}"
