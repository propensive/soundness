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
# Where to clone from; a local clone works too (TEL_REPO=file:///path/to/tel).
TEL_REPO="${TEL_REPO:-https://github.com/propensive/tel.git}"
# NOTE: the resource path deliberately avoids `stratiform/tel/` because
# Mill puts test resources on the compiler classpath, and a directory at
# `stratiform/tel/` is interpreted as a package, shadowing the `tel"…"`
# StringContext extension. The corpus lives under `stratiform/corpus/`.
TARGET="lib/stratiform/res/test/stratiform/corpus"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

git clone --depth 1 --branch "$TEL_REF" --quiet "$TEL_REPO" "$WORK/tel"

mkdir -p "$TARGET/pos" "$TARGET/neg" "$TARGET/stream"

# Fixtures maintained locally that upstream does not carry (see
# DIVERGENCES.md). A sync never deletes them; everything else in a category
# is replaced by upstream's copy, so a fixture upstream renames or removes
# disappears here too. `tels.tel` and `tels.bintel.hex` are maintained by
# hand and are not touched.
LOCAL_ONLY=(
  pos/pragma-full-form
  pos/pragma-layers-with-signature
  pos/pragma-pinned-tels
  pos/pragma-reference-bare
  pos/pragma-reference-tag
  pos/pragma-reference-version
  neg/e121-plus-alone
  neg/e121-reference-bad-grammar
  neg/e121-reference-leading-zero-version
  neg/e122-layer-after-signature
  neg/e122-layers-unaccompanied
  neg/e122-pragma-extra-atoms
  neg/e121-sigil-not-final
  neg/e310-schema-plus-sigil
)

is_local_only() {
  local stem="$1"
  local keep
  for keep in "${LOCAL_ONLY[@]}"; do
    [[ "$keep" == "$stem" ]] && return 0
  done
  return 1
}

sync_category() {
  local category="$1"
  local f stem
  for f in "$TARGET/$category"/*.tel "$TARGET/$category"/*.check; do
    [[ -e "$f" ]] || continue
    stem="$category/$(basename "${f%.*}")"
    is_local_only "$stem" || rm -f "$f"
  done
  cp "$WORK/tel/ref/tel/test/$category/"*.tel   "$TARGET/$category/"
  cp "$WORK/tel/ref/tel/test/$category/"*.check "$TARGET/$category/"
}

sync_category pos     # positive corpus
sync_category neg     # negative corpus
sync_category stream  # multi-document stream corpus (§6.1)

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
