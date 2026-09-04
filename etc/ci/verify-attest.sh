#!/usr/bin/env bash
#
# Verify the local-CI attestation for HEAD (or a given commit).
# Used by `make verify-attest` locally AND by GitHub Actions.
#
# The note is looked up by the commit's *filtered tree* (its tree minus every
# .dockerignore-excluded path — see etc/ci/compute-filtered-tree.sh), so an
# attestation survives squash/rebase/amend as long as the input set is the
# same. Notes attached directly to the commit (the pre-tree-keyed scheme) are
# still accepted as a fallback.
#
# Exit codes:
#   0  attestation present, signature valid, input digest matches
#   1  no attestation, bad signature, or digest mismatch

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

COMMIT="${1:-HEAD}"
COMMIT_SHA=$(git rev-parse "$COMMIT")
NOTES_REF="refs/notes/ci-attestation"
ALLOWED="$PWD/.ci/allowed_signers"

if [[ ! -f "$ALLOWED" ]]; then
  echo "verify-attest: .ci/allowed_signers is missing" >&2
  exit 1
fi

TREE=$(etc/ci/compute-filtered-tree.sh "$COMMIT_SHA")

if git notes --ref="$NOTES_REF" show "$TREE" >/dev/null 2>&1; then
  NOTE_KEY="$TREE"
elif git notes --ref="$NOTES_REF" show "$COMMIT_SHA" >/dev/null 2>&1; then
  NOTE_KEY="$COMMIT_SHA"
else
  echo "verify-attest: no attestation note on $NOTES_REF for filtered tree $TREE (or commit $COMMIT_SHA)" >&2
  echo "  (did you forget to push notes with \`make push\`, or run \`make attest\`?)" >&2
  exit 1
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
ENV_FILE="$TMP/envelope.json"
STMT="$TMP/statement.canonical"
SIG="$TMP/signature.pem"

git notes --ref="$NOTES_REF" show "$NOTE_KEY" > "$ENV_FILE"

# Split the envelope into a canonical statement (for verification),
# the detached signature, the signer principal, the claimed digest and the
# claimed filtered tree ("-" for envelopes written before trees were recorded).
read -r SIGNER CLAIMED_DIGEST CLAIMED_TREE < <(python3 - "$ENV_FILE" "$STMT" "$SIG" <<'PY'
import json, sys
env_path, stmt_path, sig_path = sys.argv[1], sys.argv[2], sys.argv[3]
with open(env_path) as f:
    env = json.load(f)
statement = env["statement"]
signature = env["signature"]
with open(stmt_path, "wb") as out:
    out.write(json.dumps(
        statement, sort_keys=True, separators=(",", ":"), ensure_ascii=False
    ).encode("utf-8"))
with open(sig_path, "w") as out:
    out.write(signature)
digest = statement["subject"][0]["digest"]
print(statement["predicate"]["ranBy"], digest["sha256"], digest.get("gitTree", "-"))
PY
)

# Verify the signature against allowed_signers under namespace `soundness-ci`.
if ! ssh-keygen -Y verify \
      -f "$ALLOWED" \
      -I "$SIGNER" \
      -n soundness-ci \
      -s "$SIG" < "$STMT" >/dev/null 2>&1; then
  echo "verify-attest: signature INVALID (signer=$SIGNER)" >&2
  exit 1
fi

# Recompute the input digest at the verified commit and compare.
ACTUAL_DIGEST=$(etc/ci/compute-input-digest.sh "$COMMIT_SHA")
if [[ "$ACTUAL_DIGEST" != "$CLAIMED_DIGEST" ]]; then
  echo "verify-attest: input digest MISMATCH" >&2
  echo "  claimed: $CLAIMED_DIGEST" >&2
  echo "  actual:  $ACTUAL_DIGEST" >&2
  exit 1
fi

# Envelopes that record the filtered tree must name the one we recomputed.
if [[ "$CLAIMED_TREE" != "-" && "$CLAIMED_TREE" != "$TREE" ]]; then
  echo "verify-attest: filtered tree MISMATCH" >&2
  echo "  claimed: $CLAIMED_TREE" >&2
  echo "  actual:  $TREE" >&2
  exit 1
fi

echo "verify-attest: OK (commit=$COMMIT_SHA tree=$TREE signer=$SIGNER digest=$ACTUAL_DIGEST)" >&2
