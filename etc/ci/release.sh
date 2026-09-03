#!/usr/bin/env bash
#
# Publish a release to GitHub Releases from this machine.
#
# A local pipeline that matches the `make attest` philosophy: the SSH-signed attestation note on
# `refs/notes/ci-attestation` is the trust boundary, not a CI runner. Publishing goes to GitHub
# Releases rather than Maven Central (whose caps on file count, size and deployment frequency
# ruled out publishing every component): the tagged version becomes one release whose assets are
# the jars of every component and every platform cross — `release.stage` in build.mill — each
# named `<artifactId>-<version>.jar`. No bundles, source or javadoc jars, or checksum files are
# published; GitHub records a SHA-256 digest per asset.
#
# Usage: ./etc/ci/release.sh X.Y.Z   (or `make release VERSION=X.Y.Z`)
#
# Requirements:
#   - the GitHub CLI (`gh`), authenticated with permission to create releases on the repository
#     (override the repository with SOUNDNESS_RELEASE_REPO=owner/repo);
#   - git configured to sign tags (`git tag -s`).
#
# Order of operations, so that nothing partial is ever visible: the release is created as a
# DRAFT against the released commit and every jar is uploaded into it; only then is the signed
# tag pushed and the draft published. A failure before that point deletes the draft and the
# local tag, leaving origin untouched.

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

VERSION="${1:-}"
REPO="${SOUNDNESS_RELEASE_REPO:-propensive/soundness}"

if [[ -z "$VERSION" ]]; then
  echo "Usage: $0 X.Y.Z" >&2; exit 1
fi
if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "release: VERSION must be X.Y.Z (got '$VERSION')" >&2; exit 1
fi

# ---------------------------- GUARDS ----------------------------

if ! command -v gh >/dev/null 2>&1; then
  echo "release: the GitHub CLI (gh) is required" >&2; exit 1
fi
if ! gh auth status >/dev/null 2>&1; then
  echo "release: gh is not authenticated; run 'gh auth login'" >&2; exit 1
fi

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
if gh release view "$VERSION" --repo "$REPO" >/dev/null 2>&1; then
  echo "release: a release named $VERSION already exists on $REPO" >&2; exit 1
fi

./etc/ci/verify-attest.sh "$HEAD_SHA"
./mill groupCheck.validate

# ---------------------------- STAGE ----------------------------

# Tag locally first so the tag object is signed. It is not pushed until every asset has been
# uploaded — a failed release must not leave a published tag for a version that does not exist.
git tag -s "$VERSION" -m "Version $VERSION"

drafted=""

rollback() {
  if [[ -n "$drafted" ]]; then
    gh release delete "$VERSION" --repo "$REPO" --yes >/dev/null 2>&1 || true
    echo "release: deleted the draft release $VERSION" >&2
  fi
  git tag -d "$VERSION" >/dev/null 2>&1 || true
  echo "release: removed local tag $VERSION" >&2
}

fail() {
  echo "release: $1" >&2
  rollback
  exit 1
}

# Drive the published version explicitly rather than letting each module re-derive it from git.
# `publishVersion` reads this (build.mill); it is the single source of truth for the release, so
# every asset carries exactly $VERSION regardless of `out/` cache state or git-describe quirks.
export SOUNDNESS_RELEASE_VERSION="$VERSION"

# Guard: every released module must resolve to exactly $VERSION before anything leaves the machine.
# Probes both compiler plugins (which shipped a stale 0.63.0 in the 0.64.0 bundle) plus a component
# and its crosses; the env var makes all modules resolve identically, so a few suffice.
probes="beneficence.plugin larceny.plugin rudiments.core rudiments.core.js rudiments.core.native"
for module in $probes; do
  resolved=$(./mill show "$module.publishVersion" | tr -d '"')
  if [[ "$resolved" != "$VERSION" ]]; then
    fail "$module.publishVersion=$resolved, expected $VERSION; aborting"
  fi
done

# Build every released jar into one directory, named as its release asset.
if ! ./mill release.stage; then
  fail "building the release jars failed"
fi

STAGE_DIR="out/release/stage.dest"
mapfile -t jars < <(find "$STAGE_DIR" -maxdepth 1 -name '*.jar' | sort)
count=${#jars[@]}

if (( count == 0 )); then
  fail "release.stage produced no jars"
fi

for jar in "${jars[@]}"; do
  if [[ "$(basename "$jar")" != *"-$VERSION.jar" ]]; then
    fail "staged jar $(basename "$jar") does not carry version $VERSION"
  fi
done

echo "release: staged $count jars for $VERSION"

# ---------------------------- PUBLISH ----------------------------

notes="Soundness $VERSION.

Every component of every library is attached as its own jar, \`<artifactId>-$VERSION.jar\`, \
alongside the Scala.js (\`_sjs1_3\`) and Scala Native (\`_native0.5_3\`) cross-builds of the \
platform-capable components. The build and test run behind this release are attested by the \
signed note on \`refs/notes/ci-attestation\` for commit $HEAD_SHA."

if ! gh release create "$VERSION" --repo "$REPO" --draft --target "$HEAD_SHA" \
       --title "Soundness $VERSION" --notes "$notes" >/dev/null; then
  fail "could not create the draft release"
fi
drafted="yes"

# Upload in batches: one `gh release upload` per fifty jars keeps each API session short enough
# to retry cheaply, and `--clobber` makes a retried batch idempotent.
batch=50
for (( start = 0; start < count; start += batch )); do
  if ! gh release upload "$VERSION" --repo "$REPO" --clobber "${jars[@]:start:batch}" \
         >/dev/null; then
    fail "uploading jars $((start + 1))-$((start + batch > count ? count : start + batch)) failed"
  fi
  echo "release: uploaded $((start + batch > count ? count : start + batch))/$count jars"
done

# Verify that GitHub holds exactly the staged set before anything becomes visible.
uploaded=$(gh release view "$VERSION" --repo "$REPO" --json assets --jq '.assets | length')
if [[ "$uploaded" != "$count" ]]; then
  fail "the draft release has $uploaded assets but $count jars were staged"
fi

# Every jar is in place: push the signed tag (so publishing the draft adopts it rather than
# creating an unsigned one), publish, and push the attestation notes.
if ! git push origin "refs/tags/$VERSION"; then
  fail "could not push tag $VERSION"
fi

if ! gh release edit "$VERSION" --repo "$REPO" --draft=false >/dev/null; then
  echo "release: tag $VERSION is pushed but the release is still a draft; publish it by hand" >&2
  exit 1
fi

git push origin refs/notes/ci-attestation

echo "release: $VERSION published to https://github.com/$REPO/releases/tag/$VERSION ($count jars)."
