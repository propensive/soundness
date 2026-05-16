#!/usr/bin/env bash
#
# Verify the local-CI attestation note attached to HEAD (or a given commit).
# Used by `make verify-attest` locally AND by GitHub Actions.
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

if ! git notes --ref="$NOTES_REF" show "$COMMIT_SHA" >/dev/null 2>&1; then
  echo "verify-attest: no attestation note for $COMMIT_SHA on $NOTES_REF" >&2
  echo "  (did you forget to push notes with \`make push\`, or run \`make attest\`?)" >&2
  exit 1
fi

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
ENV_FILE="$TMP/envelope.json"
STMT="$TMP/statement.canonical"
SIG="$TMP/signature.pem"

git notes --ref="$NOTES_REF" show "$COMMIT_SHA" > "$ENV_FILE"

# Split the envelope into a canonical statement (for verification),
# the detached signature, the signer principal, and the claimed digest.
read -r SIGNER CLAIMED_DIGEST < <(python3 - "$ENV_FILE" "$STMT" "$SIG" <<'PY'
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
print(statement["predicate"]["ranBy"], statement["subject"][0]["digest"]["sha256"])
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

echo "verify-attest: OK (commit=$COMMIT_SHA signer=$SIGNER digest=$ACTUAL_DIGEST)" >&2
