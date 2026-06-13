#!/usr/bin/env bash
#
# Compile the reusable native runner stubs (one per platform) with `cargo zigbuild`.
#
# These are the small (~0.5 MB), generic, version-independent launchers. An application
# binary is built by appending the app's JAR to the right stub; the stubs themselves are
# reusable across applications and versions. This build is deliberately *separate* from the
# Soundness (Mill) build — runners only change when the Rust source under `lib/ethereal`
# changes, and are published independently by `runners-release.sh`.
#
# Usage: ./etc/ci/runners-build.sh [output-dir]      (default: dist/runners)
#
# Produces <output-dir>/runner-<label>[.exe] for each platform.

set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

OUT="${1:-dist/runners}"
ETHDIR="lib/ethereal"

# triple|label|binary — the platforms the runner is cross-compiled for.
TARGETS=(
  "x86_64-pc-windows-gnu|windows-x64|runner.exe"
  "x86_64-unknown-linux-gnu|linux-x64|runner"
  "aarch64-unknown-linux-gnu|linux-arm64|runner"
  "x86_64-apple-darwin|macos-x64|runner"
  "aarch64-apple-darwin|macos-arm64|runner"
)

if ! command -v cargo >/dev/null 2>&1; then
  echo "runners-build: cargo (with the zigbuild subcommand) is required" >&2; exit 1
fi

target_dir=$(mktemp -d)
trap 'rm -rf "$target_dir"' EXIT

triple_args=()
for entry in "${TARGETS[@]}"; do
  IFS='|' read -r triple _ _ <<< "$entry"
  triple_args+=(--target "$triple")
done

echo "runners-build: cross-compiling ${#TARGETS[@]} runner stubs (release)…"
cargo zigbuild --release \
  --manifest-path "$ETHDIR/Cargo.toml" \
  --target-dir "$target_dir" \
  "${triple_args[@]}"

mkdir -p "$OUT"
for entry in "${TARGETS[@]}"; do
  IFS='|' read -r triple label binary <<< "$entry"
  ext=""; [[ "$binary" == *.exe ]] && ext=".exe"
  cp -f "$target_dir/$triple/release/$binary" "$OUT/runner-$label$ext"
  chmod +x "$OUT/runner-$label$ext"
done

echo "runners-build: built into $OUT:"
ls -la "$OUT"/runner-*
