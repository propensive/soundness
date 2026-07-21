#!/usr/bin/env bash
#
# Publish a signed release to Maven Central from this machine.
#
# Replaces the previous GitHub Actions publish.yml workflow with a local
# pipeline that matches the existing `make attest` philosophy: the
# SSH-signed attestation note on `refs/notes/ci-attestation` is the trust
# boundary, not the CI runner.
#
# Usage: ./etc/ci/release.sh X.Y.Z   (or `make release VERSION=X.Y.Z`)
#
# Required secrets (read from macOS keychain — see SECRETS section below):
#   - soundness.sonatype.username
#   - soundness.sonatype.password
#   - soundness.pgp.secret.base64
#   - soundness.pgp.passphrase

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

VERSION="${1:-}"
if [[ -z "$VERSION" ]]; then
  echo "Usage: $0 X.Y.Z" >&2; exit 1
fi
if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "release: VERSION must be X.Y.Z (got '$VERSION')" >&2; exit 1
fi

# ---------------------------- GUARDS ----------------------------

if ! git diff --quiet || ! git diff --cached --quiet; then
  echo "release: working tree is dirty; commit or stash first" >&2; exit 1
fi

HEAD_SHA=$(git rev-parse HEAD)

if git rev-parse "refs/tags/$VERSION" >/dev/null 2>&1; then
  echo "release: tag $VERSION already exists locally" >&2; exit 1
fi
if git ls-remote --exit-code --tags origin "refs/tags/$VERSION" >/dev/null 2>&1; then
  echo "release: tag $VERSION already exists on origin" >&2; exit 1
fi

./etc/ci/verify-attest.sh "$HEAD_SHA"
./mill groupCheck.validate

# ---------------------------- SECRETS ----------------------------
#
# Default: macOS keychain. Set the four entries once with:
#   security add-generic-password -a soundness-release \
#     -s soundness.sonatype.username -w 'YOUR_USERNAME'
#   security add-generic-password -a soundness-release \
#     -s soundness.sonatype.password -w 'YOUR_PASSWORD'
#   security add-generic-password -a soundness-release \
#     -s soundness.pgp.secret.base64 -w 'BASE64_PGP_SECRET'
#   security add-generic-password -a soundness-release \
#     -s soundness.pgp.passphrase    -w 'YOUR_PASSPHRASE'
#
# Alternative secret sources (swap the `read_secret` function below):
#   - 1Password CLI: `op read 'op://Private/Soundness/$1'`
#   - gitignored .env.release file: `source .env.release` and read env vars
#

read_secret() {
  security find-generic-password -a soundness-release -s "$1" -w 2>/dev/null \
    || { echo "release: missing keychain entry '$1'" >&2; exit 1; }
}

export MILL_SONATYPE_USERNAME=$(read_secret soundness.sonatype.username)
export MILL_SONATYPE_PASSWORD=$(read_secret soundness.sonatype.password)
export MILL_PGP_SECRET_BASE64=$(read_secret soundness.pgp.secret.base64)
export MILL_PGP_PASSPHRASE=$(read_secret soundness.pgp.passphrase)

# ---------------------------- PUBLISH ----------------------------

# Tag locally first so PGP can sign the tag object. Don't push the tag yet —
# if Mill publish fails, we'd be left with a published tag for a non-released
# version.
git tag -s "$VERSION" -m "Version $VERSION"

# Drive the published version explicitly rather than letting each module re-derive it from git.
# `publishVersion` reads this (build.mill); it is the single source of truth for the release, so
# every artifact carries exactly $VERSION regardless of `out/` cache state or git-describe quirks.
export SOUNDNESS_RELEASE_VERSION="$VERSION"

# Guard: every published module must resolve to exactly $VERSION before anything leaves the machine.
# Probes both compiler plugins (which shipped a stale 0.63.0 in the 0.64.0 bundle) plus a normal
# module; the env var makes all modules resolve identically, so a representative few suffice.
for module in beneficence.plugin decorum.plugin rudiments.core; do
  resolved=$(./mill show "$module.publishVersion" | tr -d '"')
  if [[ "$resolved" != "$VERSION" ]]; then
    echo "release: $module.publishVersion=$resolved, expected $VERSION; aborting" >&2
    git tag -d "$VERSION" >/dev/null
    exit 1
  fi
done

# Build the publish selector: every PublishModule EXCEPT the dormant platform crosses (the `.js`
# cross of a `scalaJs = false` module and the `.native` cross of a `scalaNative = false` module).
# Those can't cross-compile, and mill has no per-module publish skip, so subtract `dormantCrosses`
# (derived in build.mill from the capability flags) from the `__.publishArtifacts` wildcard.
dormant_file=$(mktemp)
./mill show dormantCrosses 2>/dev/null \
  | python3 -c 'import json,sys; [print(x) for x in json.load(sys.stdin)]' \
  | sort > "$dormant_file"
keep=$(./mill resolve '__.publishArtifacts' 2>/dev/null \
  | grep -E '^[a-zA-Z].*\.publishArtifacts$' \
  | sed -E 's/\.publishArtifacts$//' \
  | sort | grep -vxF -f "$dormant_file")
rm -f "$dormant_file"
if [[ -z "$keep" ]]; then
  echo "release: computed an empty publish set; aborting" >&2
  git tag -d "$VERSION" >/dev/null; exit 1
fi
publish_selector="{$(printf '%s\n' "$keep" | paste -sd, -)}.publishArtifacts"
echo "release: publishing $(printf '%s\n' "$keep" | grep -c .) modules (dormant crosses excluded)"

if ! ./mill mill.javalib.SonatypeCentralPublishModule/publishAll \
       --publishArtifacts "$publish_selector" \
       --shouldRelease true \
       --bundleName "dev.soundness-soundness:$VERSION"; then
  echo "release: publish failed; removing local tag $VERSION" >&2
  git tag -d "$VERSION" >/dev/null
  exit 1
fi

# Publish succeeded — push tag and attestation notes.
git push origin "refs/tags/$VERSION"
git push origin refs/notes/ci-attestation

echo "release: $VERSION published to Maven Central and tag pushed to origin."
