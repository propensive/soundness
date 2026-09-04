#!/usr/bin/env bash
#
# Synchronize the local ivy2 repository (`~/.ivy2/local`, which coursier — and so Mill —
# consults by default) with the Soundness jars published to GitHub Releases, so any Mill build can
# resolve `mvn"dev.propensive:<artifactId>:<version>"` without a repository configuration.
#
# Every released jar carries its own POM and ivy.xml under `META-INF/maven/…` (see `release.stage`
# in build.mill), so the jar is all that needs downloading: the descriptors are extracted from it
# into the `poms/` and `ivys/` directories of the ivy2 layout. Jars already present with the
# SHA-256 digest GitHub reports are left alone, so re-running is cheap and idempotent.
#
# Usage: ./etc/ci/sync-releases.sh [X.Y.Z]          one release, or every X.Y.Z release if omitted
#        ./etc/ci/sync-releases.sh --staged          the jars of a local `./mill release.stage`
#         (or `make sync-releases [VERSION=X.Y.Z]`)
#
# Environment: SOUNDNESS_RELEASE_REPO=owner/repo (default propensive/soundness); GITHUB_TOKEN,
# if set, is sent with API requests to lift the unauthenticated rate limit; IVY_LOCAL overrides
# the destination (default ~/.ivy2/local).

set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

exec python3 ./etc/ci/sync_releases.py "$@"
