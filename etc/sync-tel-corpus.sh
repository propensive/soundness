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
# NOTE: the resource path deliberately avoids `stratiform/tel/` because
# Mill puts test resources on the compiler classpath, and a directory at
# `stratiform/tel/` is interpreted as a package, shadowing the `tel"…"`
# StringContext extension. The corpus lives under `stratiform/corpus/`.
TARGET="lib/stratiform/res/test/stratiform/corpus"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

git clone --depth 1 --branch "$TEL_REF" --quiet \
  https://github.com/propensive/tel.git "$WORK/tel"

mkdir -p "$TARGET/pos" "$TARGET/neg" "$TARGET/stream"

# Copy positive corpus
rm -f "$TARGET/pos"/*.tel "$TARGET/pos"/*.check 2>/dev/null || true
cp "$WORK/tel/ref/tel/test/pos/"*.tel    "$TARGET/pos/"
cp "$WORK/tel/ref/tel/test/pos/"*.check  "$TARGET/pos/"

# Copy negative corpus
rm -f "$TARGET/neg"/*.tel "$TARGET/neg"/*.check 2>/dev/null || true
cp "$WORK/tel/ref/tel/test/neg/"*.tel    "$TARGET/neg/"
cp "$WORK/tel/ref/tel/test/neg/"*.check  "$TARGET/neg/"

# Copy multi-document stream corpus (§6.1)
rm -f "$TARGET/stream"/*.tel "$TARGET/stream"/*.check 2>/dev/null || true
cp "$WORK/tel/ref/tel/test/stream/"*.tel    "$TARGET/stream/"
cp "$WORK/tel/ref/tel/test/stream/"*.check  "$TARGET/stream/"

# Record the upstream commit hash for traceability.
cd "$WORK/tel"
UPSTREAM_SHA=$(git rev-parse HEAD)
cd - > /dev/null
echo "$UPSTREAM_SHA" > "$TARGET/UPSTREAM_SHA"

# Index files list the corpus case stems (without extension). Used by the
# Scala test loader to enumerate cases via ClassLoader.getResourceAsStream
# without filesystem walking.
ls "$TARGET/pos/"*.tel    | xargs -n1 basename | sed 's/\.tel$//' | sort > "$TARGET/pos.index"
ls "$TARGET/neg/"*.tel    | xargs -n1 basename | sed 's/\.tel$//' | sort > "$TARGET/neg.index"
ls "$TARGET/stream/"*.tel | xargs -n1 basename | sed 's/\.tel$//' | sort > "$TARGET/stream.index"

pos_count=$(wc -l < "$TARGET/pos.index" | tr -d ' ')
neg_count=$(wc -l < "$TARGET/neg.index" | tr -d ' ')
stream_count=$(wc -l < "$TARGET/stream.index" | tr -d ' ')
echo "Synced: $pos_count positive + $neg_count negative + $stream_count stream cases at $UPSTREAM_SHA"
