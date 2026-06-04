#!/usr/bin/env bash
#
# Run a full, from-scratch build and the test suite locally. On success, sign
# an attestation over the CI input digest and attach it as a git note on HEAD.
#
# The build runs in a throwaway git worktree checked out at HEAD, so it always
# starts from a clean build cache (no reused `out/`) and compiles exactly the
# committed tree the digest is taken over — never the dirty working tree.
#
# Environment:
#   SOUNDNESS_CI_KEY  path to the private SSH key used for signing
#                     (default: ~/.ssh/id_ed25519)
#   SOUNDNESS_CI_SKIP_BUILD=1
#                     skip the build/test step (use only when you know the
#                     inputs are unchanged from an existing attestation)
#
# Exit codes:
#   0  attestation written to refs/notes/ci-attestation for HEAD
#   1  tests failed, prerequisites missing, or signing error
#
# After success, run `make push` (or `git push && git push origin
# refs/notes/ci-attestation`) to publish the attestation alongside commits.

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

NOTES_REF="refs/notes/ci-attestation"
HEAD_SHA=$(git rev-parse HEAD)
SIGNER=$(git config user.email)

# Pick a signing key: $SOUNDNESS_CI_KEY if set, otherwise the first existing
# default (ed25519 preferred, then rsa).
if [[ -n "${SOUNDNESS_CI_KEY:-}" ]]; then
  KEY="$SOUNDNESS_CI_KEY"
elif [[ -f "$HOME/.ssh/id_ed25519" ]]; then
  KEY="$HOME/.ssh/id_ed25519"
elif [[ -f "$HOME/.ssh/id_rsa" ]]; then
  KEY="$HOME/.ssh/id_rsa"
else
  KEY=""
fi

if [[ -z "$SIGNER" ]]; then
  echo "fatal: git config user.email is empty" >&2
  exit 1
fi
if [[ -z "$KEY" || ! -f "$KEY" ]]; then
  echo "fatal: no signing key found (tried \$SOUNDNESS_CI_KEY, ~/.ssh/id_ed25519, ~/.ssh/id_rsa)" >&2
  exit 1
fi
if ! grep -q "^$SIGNER " .ci/allowed_signers 2>/dev/null; then
  echo "fatal: $SIGNER is not listed in .ci/allowed_signers" >&2
  exit 1
fi

DIGEST=$(etc/ci/compute-input-digest.sh)
echo "input digest: $DIGEST" >&2
echo "commit:       $HEAD_SHA" >&2
echo "signer:       $SIGNER" >&2

# Fast path 1: HEAD already has a valid attestation for this digest.
if git notes --ref="$NOTES_REF" show HEAD >/dev/null 2>&1; then
  EXISTING_DIGEST=$(git notes --ref="$NOTES_REF" show HEAD \
    | python3 -c 'import json,sys; print(json.load(sys.stdin)["statement"]["subject"][0]["digest"]["sha256"])' \
    2>/dev/null || true)
  if [[ "$EXISTING_DIGEST" == "$DIGEST" ]]; then
    if etc/ci/verify-attest.sh >/dev/null 2>&1; then
      echo "HEAD already has a valid attestation for this input digest." >&2
      exit 0
    fi
  fi
fi

# Fast path 2: a recent ancestor has an attestation with the same digest
# (i.e. only docs/CI-metadata changed). Re-use the existing signed note.
for ancestor in $(git log --format='%H' -n 50 HEAD --skip=1 2>/dev/null); do
  if ! git notes --ref="$NOTES_REF" show "$ancestor" >/dev/null 2>&1; then
    continue
  fi
  NOTE=$(git notes --ref="$NOTES_REF" show "$ancestor")
  AD=$(echo "$NOTE" \
    | python3 -c 'import json,sys; print(json.load(sys.stdin)["statement"]["subject"][0]["digest"]["sha256"])' \
    2>/dev/null || true)
  if [[ "$AD" == "$DIGEST" ]]; then
    echo "$NOTE" | git notes --ref="$NOTES_REF" add -f -F - HEAD
    echo "Re-used attestation from $ancestor (input digest unchanged)." >&2
    exit 0
  fi
done

# Slow path: do a full clean build and run the test suite locally.
#
# We build inside a throwaway detached worktree pinned to HEAD. A fresh worktree
# has no `out/`, so the compile starts from a clean cache every time, and it
# builds the exact committed tree the digest is computed over rather than the
# developer's (possibly dirty) working tree. The developer's own `out/` is left
# untouched. Output is tee'd to a log here in the original tree because the
# build is verbose and a real failure can scroll far off-screen.
if [[ "${SOUNDNESS_CI_SKIP_BUILD:-0}" != "1" ]]; then
  mkdir -p out
  LOG="out/attest-$(date -u +%Y%m%dT%H%M%SZ).log"
  WORKTREE_PARENT=$(mktemp -d)
  WORKTREE="$WORKTREE_PARENT/build"
  echo "Running full clean build + test suite in $WORKTREE; full output → $LOG" >&2
  git worktree add --detach "$WORKTREE" "$HEAD_SHA" >&2

  set +e
  (
    cd "$WORKTREE" || exit 1
    CLAUDECODE=1 ./mill --ticker false soundness.all.compile \
      && CLAUDECODE=1 ./mill --ticker false test.assembly \
      && { CLAUDECODE=1 ./mill shutdown || true; } \
      && CLAUDECODE=1 make ci
  ) 2>&1 | tee "$LOG"
  rc=${PIPESTATUS[0]}
  set -e

  # Tear the worktree down on both paths; don't add a second `trap … EXIT`, it
  # would clobber the statement-tempdir trap installed below.
  git worktree remove --force "$WORKTREE" 2>/dev/null || rm -rf "$WORKTREE"
  rm -rf "$WORKTREE_PARENT"

  if [[ $rc -ne 0 ]]; then
    echo >&2
    echo "build/test exited with $rc. Full log at $LOG" >&2
    echo "Last 80 lines of the log:" >&2
    echo "----" >&2
    tail -n 80 "$LOG" >&2
    exit $rc
  fi
fi

# Build the in-toto statement and sign it.
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
STMT="$TMP/statement.json"
ENV_FILE="$TMP/envelope.json"

python3 - "$DIGEST" "$SIGNER" "$HEAD_SHA" > "$STMT" <<'PY'
import datetime, json, sys
digest, signer, commit = sys.argv[1], sys.argv[2], sys.argv[3]
mill_version = open(".mill-version").read().strip()
now = datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
statement = {
    "_type": "https://in-toto.io/Statement/v1",
    "subject": [{"name": "soundness-ci-inputs", "digest": {"sha256": digest}}],
    "predicateType": "https://soundness.dev/local-ci/v1",
    "predicate": {
        "commands": [
            "./mill --ticker false soundness.all.compile",
            "./mill --ticker false test.assembly",
            "make ci",
        ],
        "ranAt": now,
        "ranBy": signer,
        "commit": commit,
        "tooling": {"mill": mill_version},
        "result": "pass",
    },
}
# Canonical encoding is used for signing.
import sys
sys.stdout.buffer.write(
    json.dumps(statement, sort_keys=True, separators=(",", ":"), ensure_ascii=False).encode("utf-8")
)
PY

# `ssh-keygen -Y sign` writes the signature to <file>.sig
ssh-keygen -Y sign -f "$KEY" -n soundness-ci "$STMT" >/dev/null 2>&1

# Build the envelope: { "statement": <statement>, "signature": <pem> }
python3 - "$STMT" "$STMT.sig" > "$ENV_FILE" <<'PY'
import json, sys
with open(sys.argv[1], "rb") as f:
    statement = json.loads(f.read())
with open(sys.argv[2]) as f:
    signature = f.read()
envelope = {"statement": statement, "signature": signature}
print(json.dumps(envelope, indent=2, sort_keys=True, ensure_ascii=False))
PY

git notes --ref="$NOTES_REF" add -f -F "$ENV_FILE" HEAD

echo >&2
echo "Attestation written to $NOTES_REF for $HEAD_SHA." >&2
echo "Run \`make push\` to publish commits and the attestation note." >&2
