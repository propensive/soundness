#!/usr/bin/env bash
#
# Download the reusable native runner stubs for a published version from GitHub into
# `dist/runners`, verifying each against the committed `etc/runners/<version>.tsv` manifest.
#
# Use this when the Rust toolchain isn't available to `make runners-build`: it fetches the
# exact bytes published by `make runners-release`. The runners are never stored in a JAR —
# builds and tests read them from `dist/runners` (or download them here first).
#
# Usage: ./etc/ci/runners-fetch.sh <version> [owner/repo]
#         (or `make runners-fetch RUNNERS_VERSION=X [REPO=owner/repo]`)

set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

VERSION="${1:-}"
REPO="${2:-propensive/soundness}"
TAG="runners-$VERSION"
MANIFEST="etc/runners/$VERSION.tsv"

if [[ -z "$VERSION" ]]; then
  echo "Usage: $0 <version> [owner/repo]" >&2; exit 1
fi
if [[ ! -f "$MANIFEST" ]]; then
  echo "runners-fetch: manifest $MANIFEST not found (was version $VERSION published?)" >&2; exit 1
fi

OUT="dist/runners"
mkdir -p "$OUT"
base="https://github.com/$REPO/releases/download/$TAG"

while IFS=$'\t' read -r label hash; do
  [[ -z "$label" ]] && continue
  ext=""; [[ "$label" == windows* ]] && ext=".exe"
  name="runner-$label$ext"
  echo "runners-fetch: downloading $name"
  if command -v curl >/dev/null 2>&1
  then curl -fsSL "$base/$name" -o "$OUT/$name"
  else wget -qO "$OUT/$name" "$base/$name"
  fi
  got=$( { sha256sum "$OUT/$name" 2>/dev/null || shasum -a 256 "$OUT/$name"; } | cut -d' ' -f1)
  if [[ "$got" != "$hash" ]]; then
    echo "runners-fetch: SHA-256 mismatch for $name (got $got, expected $hash)" >&2
    rm -f "$OUT/$name"; exit 1
  fi
  chmod +x "$OUT/$name"
done < "$MANIFEST"

echo "runners-fetch: $(find "$OUT" -maxdepth 1 -name 'runner-*' | wc -l | tr -d ' ') stubs verified into $OUT"
