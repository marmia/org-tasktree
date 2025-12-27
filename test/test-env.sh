SCRIPT_DIR="$(cd -- "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd -- "${SCRIPT_DIR}/.." && pwd)"

TEST_ROOT=${TEST_ROOT:-${REPO_ROOT}/test}
DB_PATH=${DB_PATH:-${TEST_ROOT}/tasktree.db}
QUERY_DIR=${QUERY_DIR:-${TEST_ROOT}/queries}
