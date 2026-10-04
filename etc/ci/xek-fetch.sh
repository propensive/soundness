#!/usr/bin/env bash
#
# Fetch the pinned `xek` builder into dist/xek, verified against etc/xek.tsv.
#
# `xek` is the single implementation of the XEK executable format, published from
# propensive/xek with the runner stubs. Soundness shells out to it (build.mill packaging tasks
# and `exoskeleton.Enclave` at test time) rather than carrying its own copy. Since xek-0.10 it is
# an XEK executable itself, published as a polyglot file which replaces itself with the native
# executable for its platform the first time it runs.

set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

PIN=etc/xek.tsv
VERSION=$(awk -F'\t' '$1=="version"{print $2}' "$PIN")
WANT=$(awk -F'\t' '$1=="xek"{print $2}' "$PIN")
[[ -n "$VERSION" && -n "$WANT" ]] || { echo "xek-fetch: bad pin $PIN" >&2; exit 1; }

# propensive/xeq is now propensive/xek: releases from 1.0.0 are tagged with the bare version,
# with the command as `xek`; those from the rename up to 0.10, `xek-<version>`, also as `xek`;
# those before it, `xeq-<version>`, as `xeq`. All are tried, newest naming first, and the pinned
# SHA-256 decides what is accepted.
BASE="https://github.com/propensive/xek/releases/download"
mkdir -p dist
TMP=dist/.xek.part
fetch() { if command -v curl >/dev/null 2>&1; then curl -fsSL "$1" -o "$TMP"; else wget -qO "$TMP" "$1"; fi; }
fetch "$BASE/$VERSION/xek" 2>/dev/null || fetch "$BASE/xek-$VERSION/xek" 2>/dev/null ||
  fetch "$BASE/xeq-$VERSION/xeq" ||
  { echo "xek-fetch: no builder published for version $VERSION" >&2; rm -f "$TMP"; exit 1; }
GOT=$( { sha256sum "$TMP" 2>/dev/null || shasum -a 256 "$TMP"; } | cut -d' ' -f1)
if [[ "$GOT" != "$WANT" ]]; then
  echo "xek-fetch: SHA-256 mismatch for xek (got $GOT, want $WANT)" >&2; rm -f "$TMP"; exit 1
fi
mv -f "$TMP" dist/xek
chmod +x dist/xek
echo "xek-fetch: dist/xek ($VERSION) verified"
