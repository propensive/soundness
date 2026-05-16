#!/usr/bin/env bash
#
# Emit the SHA-256 input digest for the soundness CI input set at HEAD
# (or another commit if passed as the first argument).
#
# Algorithm: tracked files at <commit>, filtered through .dockerignore,
# hashed via a deterministic manifest. See etc/ci/_lib.py for details.

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"
exec python3 etc/ci/_lib.py input-digest "${1:-HEAD}"
