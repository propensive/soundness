#!/usr/bin/env bash
#
# Link the wasm e2e component (tests/wasm) and run its scenarios under wasmtime.
#
# The ordinary build only *typechecks* the `.wasi` backends' deferred `invoke`
# navigation; nothing exercises the emitted WIT ABI, the Wasm-component link, or
# the runtime behaviour of the backends. This script closes that gap: one
# component, compiled against all seven `.wasi` backends and linked as a
# WasmComponent (`mill wasm.component`), is run under wasmtime once per
# scenario, each asserting on exact output. All scenarios are deterministic and
# offline (the TCP scenario talks to a loopback echo server started here); the
# HTTP scenario needs real network, so it only runs when SOUNDNESS_CI_ONLINE=1.
#
# Required host tooling (see .claude/CLAUDE.md, "CI workflow"): wasmtime,
# wasm-tools, and the scala-wasm fork of wit-bindgen.
#
# Environment:
#   SOUNDNESS_CI_JOBS     mill parallelism for the compile+link (default: 6)
#   SOUNDNESS_CI_ONLINE   1 to also run the network-touching http scenario
#
# Exit codes:
#   0    all scenarios passed
#   !=0  a scenario failed (its name and output are printed)

set -uo pipefail

JOBS="${SOUNDNESS_CI_JOBS:-6}"

# The component imports every interface the seven backends use, and wasmtime must provide them
# all at instantiation — so `-S http` and `-S inherit-network` are passed on every run, even for
# scenarios that never touch them (they grant capabilities; unused ones are inert).
FLAGS=(-W function-references,gc,exceptions -S http -S inherit-network)

command -v wasmtime >/dev/null || { echo "wasm-e2e: wasmtime is not on PATH" >&2; exit 1; }
echo "wasm-e2e: $(wasmtime --version)"

./mill --no-daemon -j "$JOBS" --ticker false wasm.component || exit 1
WASM=out/wasm/component.dest/main.wasm
[ -f "$WASM" ] || { echo "wasm-e2e: $WASM not produced" >&2; exit 1; }

failed=0
passed=0

# run <name> <expected-substring> <wasmtime args...>; stdin is inherited.
run() {
  local name="$1" expected="$2"; shift 2
  local output
  output=$(wasmtime run "${FLAGS[@]}" --env "SCENARIO=$name" "$@" "$WASM" 2>&1)
  if [ $? -eq 0 ] && printf '%s' "$output" | grep -qF "$expected"; then
    echo "wasm-e2e: pass: $name"
    passed=$((passed + 1))
  else
    echo "wasm-e2e: FAIL: $name (expected \"$expected\")" >&2
    printf '%s\n' "$output" | sed 's/^/wasm-e2e:   /' >&2
    failed=1
  fi
}

# ambience: read a variable back through wasi:cli/environment.
run clock "clock: ok" </dev/null

# aviation: two monotonic-clock readings through wasi:clocks.
run random "random: ok" </dev/null

# turbulence: stdin -> stdout/stderr through wasi:cli std streams. (A here-string, not a
# pipe: piping into the `run` function would run it in a subshell and lose the counters.)
run stdio "echo: hello wasm" <<< 'hello wasm'

# galilei: mkdir/write/read-back through wasi:filesystem, against a host --dir.
FSDIR=$(mktemp -d)
run fs "fs: wasm e2e probe" --dir "$FSDIR::/work" </dev/null
rm -rf "$FSDIR"

# coaxial: TCP connect through wasi:sockets to a loopback listener.
PORT=$((20000 + RANDOM % 20000))
python3 - "$PORT" <<'EOF' &
import socket, sys
s = socket.socket()
s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind(('127.0.0.1', int(sys.argv[1]))); s.listen(1); s.settimeout(30)
try:
    c, _ = s.accept(); c.close()
except Exception as e:
    print('wasm-e2e: tcp server:', e, file=sys.stderr)
s.close()
EOF
SERVER=$!
sleep 0.3
run tcp "tcp: connected" --env "PORT=$PORT" </dev/null
wait "$SERVER" 2>/dev/null

# telekinesis: an outgoing HTTP GET through wasi:http — needs real network.
if [ "${SOUNDNESS_CI_ONLINE:-0}" = "1" ]; then
  run http "http: 200" --env "URL=http://example.com/" </dev/null
else
  echo "wasm-e2e: skip: http (offline; set SOUNDNESS_CI_ONLINE=1 to run)"
fi

if [ "$failed" -ne 0 ]; then
  echo "wasm-e2e: FAILED" >&2
  exit 1
fi

echo "wasm-e2e: PASS ($passed scenarios)"
