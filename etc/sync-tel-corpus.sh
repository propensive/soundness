#!/usr/bin/env bash
#
# Sync the upstream TEL test corpus into lib/stratiform/src/test/data/.
# The corpus is pulled from github.com/propensive/tel under ref/tel/test/
# at the commit referenced by TEL_REF (default: main).
#
# The corpus consists of paired ".tel" input files and ".check" Rust-Debug
# AST dumps from the reference implementation. Tests use a Scala reader to
# parse the .check files into a cross-language CheckTree representation
# rather than relying on byte-equal string comparison.
#
# Run from anywhere in the repository.

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

TEL_REF="${TEL_REF:-main}"
TARGET="lib/stratiform/res/test/stratiform/tel"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

git clone --depth 1 --branch "$TEL_REF" --quiet \
  https://github.com/propensive/tel.git "$WORK/tel"

mkdir -p "$TARGET/pos" "$TARGET/neg"

# Copy positive corpus
rm -f "$TARGET/pos"/*.tel "$TARGET/pos"/*.check 2>/dev/null || true
cp "$WORK/tel/ref/tel/test/pos/"*.tel    "$TARGET/pos/"
cp "$WORK/tel/ref/tel/test/pos/"*.check  "$TARGET/pos/"

# Copy negative corpus
rm -f "$TARGET/neg"/*.tel "$TARGET/neg"/*.check 2>/dev/null || true
cp "$WORK/tel/ref/tel/test/neg/"*.tel    "$TARGET/neg/"
cp "$WORK/tel/ref/tel/test/neg/"*.check  "$TARGET/neg/"

# Record the upstream commit hash for traceability.
cd "$WORK/tel"
UPSTREAM_SHA=$(git rev-parse HEAD)
cd - > /dev/null
echo "$UPSTREAM_SHA" > "$TARGET/UPSTREAM_SHA"

pos_count=$(ls "$TARGET/pos/"*.tel | wc -l | tr -d ' ')
neg_count=$(ls "$TARGET/neg/"*.tel | wc -l | tr -d ' ')
echo "Synced: $pos_count positive + $neg_count negative cases at $UPSTREAM_SHA"
