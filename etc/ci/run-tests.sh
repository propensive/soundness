#!/usr/bin/env bash
#
# Run the umbrella test suite (soundness.Tests) with a hard wall-clock timeout.
#
# The `probably` table reporter buffers its output until the run completes, so a
# hung or deadlocked test produces no output at all — it just stalls forever.
# During `make attest` that would hang the whole build with no diagnostics. This
# wrapper bounds the run: if the suite exceeds the timeout, it dumps every
# thread's stack (so a deadlock is pinpointed) and fails, instead of hanging
# indefinitely.
#
# Environment:
#   SOUNDNESS_CI_TEST_TIMEOUT   seconds before the suite is declared hung
#                               (default: 1800 = 30 minutes)
#
# Exit codes:
#   0    suite passed
#   !=0  suite failed, or was killed after exceeding the timeout (124)

set -uo pipefail

JAR=out/test/assembly.dest/out.jar
TIMEOUT="${SOUNDNESS_CI_TEST_TIMEOUT:-1800}"

# The suite's measured peak is ~2.5 GB used / ~3.2 GB committed, so a 4 GB heap
# leaves comfortable headroom. Set it explicitly rather than inheriting the
# JVM's machine-dependent default (25% of RAM), so the run behaves identically
# regardless of host size. Override with SOUNDNESS_CI_TEST_HEAP.
HEAP="${SOUNDNESS_CI_TEST_HEAP:-4g}"

java -Xss2m "-Xmx$HEAP" -cp "$JAR" soundness.Tests &
pid=$!

# Watchdog: if the suite is still alive after $TIMEOUT, capture a full thread
# dump and kill it. We emit two dumps because the suite runs on virtual threads:
#   * jstack — platform threads and carrier→virtual-thread mounting, readable.
#   * jcmd Thread.dump_to_file -format=json — EVERY thread including *parked*
#     virtual threads with their application stacks. jstack alone cannot show an
#     unmounted virtual thread (e.g. a parasite daemon parked on a promise), so a
#     deadlock among virtual threads is invisible without this.
(
  # Run the timeout as a tracked background child with a TERM trap that reaps it.
  # If we instead ran a bare `sleep "$TIMEOUT"` and the parent later did
  # `kill "$watchdog"`, that would kill this subshell but ORPHAN the external
  # `sleep`, which keeps the inherited write-end of the caller's `… | tee` pipe
  # open — wedging `tee` (and `make attest`) until the stray sleep finally ends.
  sleep "$TIMEOUT" &
  sp=$!
  trap 'kill "$sp" 2>/dev/null; exit 0' TERM
  wait "$sp"
  if kill -0 "$pid" 2>/dev/null; then
    echo >&2
    echo "::: test suite exceeded ${TIMEOUT}s — assuming a hang; thread dumps follow :::" >&2
    echo "::: --- jstack (platform threads + carrier mounting) --- :::" >&2
    jstack "$pid" >&2 2>/dev/null || jcmd "$pid" Thread.print >&2 2>/dev/null || true
    echo "::: --- jcmd Thread.dump_to_file -format=json (all threads, incl. virtual) --- :::" >&2
    if jcmd "$pid" Thread.dump_to_file -format=json -overwrite /tmp/threaddump.json >&2 2>/dev/null
    then cat /tmp/threaddump.json >&2
    fi
    kill -9 "$pid" 2>/dev/null
    exit 124
  fi
) &
watchdog=$!

wait "$pid"
rc=$?

# Stop the watchdog if the suite finished on its own.
kill "$watchdog" 2>/dev/null
wait "$watchdog" 2>/dev/null
watchdog_rc=$?

# If the watchdog fired (exit 124), surface that as the failure.
if [[ $watchdog_rc -eq 124 ]]; then
  exit 124
fi

exit $rc
