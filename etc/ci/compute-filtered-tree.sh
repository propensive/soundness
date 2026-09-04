#!/usr/bin/env bash
#
# Emit the SHA of the filtered git tree for the soundness CI input set at HEAD
# (or another commit if passed as the first argument).
#
# This is the object attestation notes are attached to: the commit's tree with
# every .dockerignore-excluded path removed, so it identifies only the relevant
# content and is unaffected by squashes, rebases and amends. See
# etc/ci/_lib.py for details.

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"
exec python3 etc/ci/_lib.py filtered-tree "${1:-HEAD}"
