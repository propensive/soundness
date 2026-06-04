#!/usr/bin/env bash
#
# Run the umbrella test suite (soundness.Tests) with a hard wall-clock timeout.
#
# The `probably` table reporter buffers its output until the run completes, so a
# hung or deadlocked test produces no output at all — it just stalls forever.
# Inside `docker build` (img/test stage 3) that hangs the whole CI build with no
# diagnostics. This wrapper bounds the run: if the suite exceeds the timeout, it
# dumps every thread's stack (so a deadlock is pinpointed) and fails, instead of
# hanging indefinitely.
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

java -cp "$JAR" soundness.Tests &
pid=$!

# Watchdog: if the suite is still alive after $TIMEOUT, capture a full thread
# dump (deadlock diagnosis) and kill it.
(
  sleep "$TIMEOUT"
  if kill -0 "$pid" 2>/dev/null; then
    echo >&2
    echo "::: test suite exceeded ${TIMEOUT}s — assuming a hang; thread dump follows :::" >&2
    jstack "$pid" >&2 2>/dev/null || jcmd "$pid" Thread.print >&2 2>/dev/null || true
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
