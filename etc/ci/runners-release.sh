#!/usr/bin/env bash
#
# Publish the reusable native runner stubs to a versioned GitHub release, and record their
# SHA-256 hashes in a committed manifest (`etc/runners/<version>.tsv`).
#
# The stubs are version-independent and reusable, so they are published *separately* from —
# and far less frequently than — application releases (only when the Rust runner source
# changes). An application build then consumes a given runner version: a monoglot build
# downloads the one stub for its platform; an offline polyglot build downloads all five and
# embeds them; an online polyglot build embeds the five (url, hash) pairs from the manifest
# and downloads the right stub at runtime. In every case the app's JAR is appended to a bare
# stub — the stub bytes are never re-published per application.
#
# Usage: ./etc/ci/runners-release.sh <version> [owner/repo]
#         (or `make runners-release RUNNERS_VERSION=X [REPO=owner/repo]`)
#
# Side effects: uploads to the GitHub release `runners-<version>`, and writes
# `etc/runners/<version>.tsv` (commit it — it is the source of truth for the hashes that get
# embedded in online downloaders).

set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

VERSION="${1:-}"
REPO="${2:-propensive/soundness}"
TAG="runners-$VERSION"

if [[ -z "$VERSION" ]]; then
  echo "Usage: $0 <version> [owner/repo]" >&2; exit 1
fi
if ! command -v gh >/dev/null 2>&1; then
  echo "runners-release: the GitHub CLI (gh) is required" >&2; exit 1
fi

OUT="dist/runners"
./etc/ci/runners-build.sh "$OUT"

# Record the SHA-256 of every stub into the committed manifest, keyed by platform label.
# These are computed from the exact bytes uploaded below, so they always match.
mkdir -p etc/runners
MANIFEST="etc/runners/$VERSION.tsv"
: > "$MANIFEST"
for f in "$OUT"/runner-*; do
  name=$(basename "$f")
  label=${name#runner-}; label=${label%.exe}
  hash=$( { sha256sum "$f" 2>/dev/null || shasum -a 256 "$f"; } | cut -d' ' -f1)
  printf '%s\t%s\n' "$label" "$hash" >> "$MANIFEST"
done
sort -o "$MANIFEST" "$MANIFEST"
echo "runners-release: wrote $MANIFEST"

# Create the release if it doesn't exist yet, then upload the bare stubs (clobber on re-run).
if ! gh release view "$TAG" --repo "$REPO" >/dev/null 2>&1; then
  gh release create "$TAG" --repo "$REPO" --title "$TAG" \
    --notes "Reusable native runner stubs, version $VERSION"
fi
gh release upload "$TAG" --repo "$REPO" --clobber "$OUT"/runner-*

count=$(find "$OUT" -maxdepth 1 -name 'runner-*' | wc -l | tr -d ' ')
echo "runners-release: uploaded $count stubs to $REPO@$TAG"
echo "runners-release: commit $MANIFEST to record the published hashes"
