SCRIPT_DIR="$(cd -- "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd -- "${SCRIPT_DIR}/.." && pwd)"

TEST_DB_DIR=${REPO_ROOT}/.org-tasktree-tmp
DB_PATH=${TEST_DB_DIR}/tasktree.db
QUERY_DIR=${TEST_DB_DIR}/queries
