#!/usr/bin/env bash
#
# Link the wasm e2e components (tests/wasm), run their scenarios under wasmtime,
# and package them as Wasm OCI Artifacts.
#
# The ordinary build only *typechecks* the `.wasi` backends' deferred `invoke`
# navigation; nothing exercises the emitted WIT ABI, the Wasm-component link, or
# the runtime behaviour of the backends. This script closes that gap in three
# stages:
#
#   1. The `e2e` component — compiled against all seven `.wasi` backends and
#      linked as a WasmComponent (`mill wasm.component`) — is run under wasmtime
#      once per scenario, each asserting on exact output.
#   2. Both components are packaged as Wasm OCI Artifacts (`mill wasm.image`,
#      `wasm.httpImage`), and each archive is checked against the artifact
#      layout: media types, digests, platform, and the world's imports and
#      exports. The `e2e` layer is then extracted and run again, so the bytes
#      that ship are proven to be the bytes that work.
#   3. The `http` component — which exports `wasi:http/incoming-handler`, the
#      only thing that links telekinesis' `WasiHttpServer` — is served from its
#      extracted layer under `wasmtime serve` and asked to echo a request.
#
# All scenarios are deterministic and offline (the TCP scenario talks to a
# loopback echo server started here, and the HTTP server scenario to a loopback
# `wasmtime serve`); the outgoing-HTTP scenario needs real network, so it only
# runs when SOUNDNESS_CI_ONLINE=1.
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
# `RUNLABEL` renames a run in the report, for when the same scenario is run against a second
# component (the one extracted from the OCI artifact).
run() {
  local name="$1" expected="$2"; shift 2
  local label="${RUNLABEL:-$name}"
  local output
  output=$(wasmtime run "${FLAGS[@]}" --env "SCENARIO=$name" "$@" "$WASM" 2>&1)
  if [ $? -eq 0 ] && printf '%s' "$output" | grep -qF "$expected"; then
    echo "wasm-e2e: pass: $label"
    passed=$((passed + 1))
  else
    echo "wasm-e2e: FAIL: $label (expected \"$expected\")" >&2
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

# --- OCI packaging --------------------------------------------------------------------------
#
# The component is only deployable if something can describe it as an image. `mill wasm.image`
# wraps each linked component in a Wasm OCI Artifact (embarcadero) whose config records the world's
# imports and exports (xenophile's WIT parser). The assertions below check the artifact against the
# spec's shape and — the part that matters — that the layer bytes are still a runnable component,
# by extracting them from the archive and running them.

./mill --no-daemon -j "$JOBS" --ticker false wasm.image wasm.httpImage || exit 1

# check <label> <archive> <expected-export> <expected-target> <extract-to>
# Validates the artifact's media types, platform and component metadata, and writes out its layer.
check() {
  local label="$1" archive="$2" export="$3" target="$4" extract="$5"
  local output
  output=$(python3 - "$archive" "$export" "$target" "$extract" <<'PY' 2>&1
import json, sys, tarfile, hashlib

archive, expected_export, expected_target, extract = sys.argv[1:5]
tar = tarfile.open(archive)
blob = lambda digest: tar.extractfile('blobs/sha256/' + digest.split(':')[1]).read()

layout = json.loads(tar.extractfile('oci-layout').read())
assert layout['imageLayoutVersion'].startswith('1.'), layout

index = json.loads(tar.extractfile('index.json').read())
assert index['mediaType'] == 'application/vnd.oci.image.index.v1+json', index
descriptor = index['manifests'][0]

manifest_bytes = blob(descriptor['digest'])
assert hashlib.sha256(manifest_bytes).hexdigest() == descriptor['digest'].split(':')[1]
manifest = json.loads(manifest_bytes)
assert manifest['mediaType'] == 'application/vnd.oci.image.manifest.v1+json', manifest
assert manifest['config']['mediaType'] == 'application/vnd.wasm.config.v0+json', manifest

layers = manifest['layers']
assert len(layers) == 1 and layers[0]['mediaType'] == 'application/wasm', layers

config_bytes = blob(manifest['config']['digest'])
assert hashlib.sha256(config_bytes).hexdigest() == manifest['config']['digest'].split(':')[1]
config = json.loads(config_bytes)
assert config['architecture'] == 'wasm' and config['os'] == 'wasip2', config
assert config['layerDigests'] == [layers[0]['digest']], config
assert expected_export in config['component']['exports'], config
assert config['component']['imports'], config
assert config['component'].get('target', '') == expected_target, config

payload = blob(layers[0]['digest'])
assert hashlib.sha256(payload).hexdigest() == layers[0]['digest'].split(':')[1]
assert len(payload) == layers[0]['size']
# An uncompressed layer, so the config's diff-id-equivalent digest names the bytes verbatim.
assert payload[:4] == b'\x00asm', payload[:8]
open(extract, 'wb').write(payload)
print('ok %d imports' % len(config['component']['imports']))
PY
)
  if [ $? -eq 0 ]; then
    echo "wasm-e2e: pass: $label ($output)"
    passed=$((passed + 1))
  else
    echo "wasm-e2e: FAIL: $label" >&2
    printf '%s\n' "$output" | sed 's/^/wasm-e2e:   /' >&2
    failed=1
  fi
}

OCIDIR=$(mktemp -d)
check "oci-artifact (e2e)" out/wasm/image.dest/image.tar \
  "wasi:cli/run@0.2.0" "" "$OCIDIR/e2e.wasm"
check "oci-artifact (http)" out/wasm/httpImage.dest/image.tar \
  "wasi:http/incoming-handler@0.2.0" "wasi:http/proxy@0.2.0" "$OCIDIR/http.wasm"

# The layer extracted from the archive must still be the component the scenarios ran.
if [ -f "$OCIDIR/e2e.wasm" ]; then
  LINKED="$WASM"
  WASM="$OCIDIR/e2e.wasm"
  RUNLABEL="packaged-clock" run clock "clock: ok" </dev/null
  unset RUNLABEL
  WASM="$LINKED"
fi

# telekinesis' WasiHttpServer, served from the packaged component under `wasmtime serve` — the
# only scenario that exercises the *incoming* half of the WASI HTTP backend.
if [ -f "$OCIDIR/http.wasm" ]; then
  HTTPPORT=$((20000 + RANDOM % 20000))
  wasmtime serve -W function-references,gc,exceptions \
    --addr "127.0.0.1:$HTTPPORT" "$OCIDIR/http.wasm" >"$OCIDIR/serve.log" 2>&1 &
  SERVE=$!

  output=$(python3 - "$HTTPPORT" <<'PY' 2>&1
import sys, time, urllib.request, urllib.error

url = 'http://127.0.0.1:%s/echo' % sys.argv[1]

# The server is a freshly-spawned process; retry briefly rather than racing its bind.
for attempt in range(40):
    try:
        with urllib.request.urlopen(url, data=b'hello wasm', timeout=5) as response:
            print(response.read().decode('utf-8'))
            sys.exit(0)
    except urllib.error.HTTPError as error:
        print('HTTP %d: %s' % (error.code, error.read().decode('utf-8', 'replace')))
        sys.exit(1)
    except Exception:
        time.sleep(0.25)

print('no response from %s' % url)
sys.exit(1)
PY
)
  if [ $? -eq 0 ] && printf '%s' "$output" | grep -qF "POST /echo" \
     && printf '%s' "$output" | grep -qF "hello wasm"; then
    echo "wasm-e2e: pass: http-server"
    passed=$((passed + 1))
  else
    echo "wasm-e2e: FAIL: http-server (expected the request line and body echoed)" >&2
    printf '%s\n' "$output" | sed 's/^/wasm-e2e:   /' >&2
    sed 's/^/wasm-e2e:   serve: /' "$OCIDIR/serve.log" >&2
    failed=1
  fi

  kill "$SERVE" 2>/dev/null
  wait "$SERVE" 2>/dev/null
fi

rm -rf "$OCIDIR"

if [ "$failed" -ne 0 ]; then
  echo "wasm-e2e: FAILED" >&2
  exit 1
fi

echo "wasm-e2e: PASS ($passed scenarios)"
