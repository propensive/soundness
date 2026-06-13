#!/usr/bin/env bash
#
# Publish download-on-demand binaries for the ethereal-hello example to a GitHub
# release, and generate the matching ziggurat launcher script.
#
# This is the network-facing half of the "download-on-demand" packaging method
# (the deterministic, in-process half is `ethereal.example.distributable`, which
# stages the binaries and emits the launcher). Keeping them separate means the
# launcher's embedded SHA-256s are always computed from the exact bytes that get
# uploaded.
#
# Usage: ./etc/ci/xeq-release.sh <version> [owner/repo] [tag]
#         (or `make xeq-release VERSION=X.Y.Z`)
#
# Produces (under ./dist):
#   - xeq-<label>-<version>   the self-contained per-platform binaries (uploaded)
#   - hello                   the launcher script (distribute this)
#
# The launcher embeds, per platform, the asset URL and its SHA-256. On first run
# it detects the platform, downloads the right binary, verifies it, overwrites
# itself, and execs.

set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

VERSION="${1:-}"
REPO="${2:-propensive/soundness}"
TAG="${3:-$VERSION}"

if [[ -z "$VERSION" ]]; then
  echo "Usage: $0 <version> [owner/repo] [tag]" >&2; exit 1
fi
if ! command -v gh >/dev/null 2>&1; then
  echo "xeq-release: the GitHub CLI (gh) is required" >&2; exit 1
fi

BASE="https://github.com/$REPO/releases/download/$TAG/"

# Build the per-platform binaries and the launcher into ./dist. The base URL
# baked into the launcher must match where we upload to below.
./mill ethereal.example.distributable --version "$VERSION" --baseUrl "$BASE"

# Create the release if it doesn't exist yet, then upload (clobber on re-run so
# the script is idempotent).
if ! gh release view "$TAG" --repo "$REPO" >/dev/null 2>&1; then
  gh release create "$TAG" --repo "$REPO" --title "$TAG" \
    --notes "Ziggurat download-on-demand binaries for $TAG"
fi
gh release upload "$TAG" --repo "$REPO" --clobber dist/xeq-*

count=$(find dist -maxdepth 1 -name 'xeq-*' | wc -l | tr -d ' ')
echo "xeq-release: uploaded $count binaries to $REPO@$TAG"
echo "xeq-release: launcher written to ./dist/hello"
