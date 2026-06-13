#!/usr/bin/env bash
#
# Build the ethereal-hello example's polyglot online launcher.
#
# Unlike the old download-on-demand flow, nothing app-specific is published here. The
# reusable bare runner stubs are released independently as `runners-<version>` (see
# `make runners-release`). This embeds the application JAR once into a polyglot launcher
# that, on first run, downloads the one stub it needs from that release, verifies it
# against the committed `etc/runners/<version>.tsv` manifest, appends the embedded JAR,
# and execs.
#
# Usage: ./etc/ci/xeq-build.sh <runners-version>
#         (or `make xeq-build RUNNERS_VERSION=X`)
#
# Produces ./dist/hello — the launcher to distribute.

set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

RUNNERS_VERSION="${1:-}"

if [[ -z "$RUNNERS_VERSION" ]]; then
  echo "Usage: $0 <runners-version>" >&2; exit 1
fi
if [[ ! -f "etc/runners/$RUNNERS_VERSION.tsv" ]]; then
  echo "xeq-build: etc/runners/$RUNNERS_VERSION.tsv not found — run \`make runners-release RUNNERS_VERSION=$RUNNERS_VERSION\` first" >&2
  exit 1
fi

./mill ethereal.example.distributable --runnersVersion "$RUNNERS_VERSION"

echo "xeq-build: launcher written to ./dist/hello"
