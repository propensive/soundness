#!/usr/bin/env bash
#
# Fetch the pinned `xeq` builder script into dist/xeq, verified against etc/xeq.tsv.
#
# `xeq` is the single implementation of the XEQ executable format, published from
# propensive/xeq with the runner stubs. Soundness shells out to it (build.mill packaging tasks
# and `exoskeleton.Enclave` at test time) rather than carrying its own copy.

set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

PIN=etc/xeq.tsv
VERSION=$(awk -F'\t' '$1=="version"{print $2}' "$PIN")
WANT=$(awk -F'\t' '$1=="xeq"{print $2}' "$PIN")
[[ -n "$VERSION" && -n "$WANT" ]] || { echo "xeq-fetch: bad pin $PIN" >&2; exit 1; }

URL="https://github.com/propensive/xeq/releases/download/xeq-$VERSION/xeq"
mkdir -p dist
TMP=dist/.xeq.part
if command -v curl >/dev/null 2>&1; then curl -fsSL "$URL" -o "$TMP"; else wget -qO "$TMP" "$URL"; fi
GOT=$( { sha256sum "$TMP" 2>/dev/null || shasum -a 256 "$TMP"; } | cut -d' ' -f1)
if [[ "$GOT" != "$WANT" ]]; then
  echo "xeq-fetch: SHA-256 mismatch for xeq (got $GOT, want $WANT)" >&2; rm -f "$TMP"; exit 1
fi
mv -f "$TMP" dist/xeq
chmod +x dist/xeq
echo "xeq-fetch: dist/xeq ($VERSION) verified"
