#!/usr/bin/env bash
# Runs fume in a sandbox of its own:  etc/ci/fume-run.sh <fume run arguments...>
#
# A test run gets a private daemon (`XDG_RUNTIME_DIR`, which ethereal-built daemons root
# themselves in, so a daemon started by another checkout or another session is never
# consulted) and a private temporary directory — `java.io.tmpdir` for the suites' JVM, which is
# what `temporaryDirectories.systemTemporaryDirectory` resolves, and `TMPDIR` for whatever they
# shell out to — and both are removed when the run ends, however it ends. Nothing a suite
# forgets to delete outlives the run: five days of suites leaving their scratch directories
# behind had put 56 GiB into the user's temp directory (2026-10-08). The JVM options ride in
# `JAVA_TOOL_OPTIONS`, which fume's daemon reads as it starts; anything already there is kept.
# Native access is enabled because guillotine's pseudo-terminals and enigmatic's cloaks call
# restricted methods of the foreign function API, which the JDK otherwise warns about.
set -uo pipefail

sandbox=$(mktemp -d)
mkdir -m 700 "$sandbox/runtime" "$sandbox/tmp"
export XDG_RUNTIME_DIR="$sandbox/runtime"
export TMPDIR="$sandbox/tmp"
export JAVA_TOOL_OPTIONS="${JAVA_TOOL_OPTIONS:-} -Djava.io.tmpdir=$sandbox/tmp --enable-native-access=ALL-UNNAMED"

cleanup() {
  fume quit >/dev/null 2>&1 || true
  rm -rf "$sandbox"
}
trap cleanup EXIT

fume run "$@"
