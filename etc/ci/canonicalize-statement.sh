#!/usr/bin/env bash
#
# Read a JSON document on stdin, write its canonical encoding to stdout.
# Canonical = sorted keys, no insignificant whitespace, UTF-8.

set -euo pipefail
exec python3 "$(dirname "$0")/_lib.py" canonicalize
