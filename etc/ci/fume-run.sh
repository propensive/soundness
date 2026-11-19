#!/usr/bin/env bash
# Runs fume:  etc/ci/fume-run.sh <fume run arguments...>
#
# Every suite runs in a JVM of its own (`--fork`), in this directory and with this environment,
# through the user's own fume daemon, whose node is a leaf of the machine's one Pyrocosm hub. fume
# gives each suite's JVM a temporary directory of its own — its `java.io.tmpdir`, and `TMPDIR`
# for whatever it starts — and deletes it once the JVM has exited, however it ended, so nothing a
# suite forgets to delete outlives its run: five days of suites leaving their scratch directories
# behind had once put 56 GiB into the user's temporary directory (2026-10-08). It also enables
# native access for each suite's JVM, since guillotine's pseudo-terminals and enigmatic's cloaks
# call restricted methods of the foreign function API, which the JDK otherwise warns about.
set -uo pipefail

exec fume run --fork "$@"
