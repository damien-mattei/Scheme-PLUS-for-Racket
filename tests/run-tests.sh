#!/usr/bin/env bash
# Run the test runner from the tests/ directory using the exact command
# requested by the maintainer: /usr/racket/bin/racket -f run-tests.rkt

set -euo pipefail

# Change to the script directory (so -f run-tests.rkt is relative to tests/)
# Keep it simple and robust for normal use.
cd "$(dirname "$0")"

exec /usr/racket/bin/racket -f run-tests.rkt
