#!/usr/bin/env bash
set -e

dir="$(cd "$(dirname "$0")/.." && pwd)"
res="$dir/res"
output="${1:-$res/installer}"

bat="$res/polyglot.bat"
ps1="$res/polyglot.ps1"
sh="$res/polyglot.sh"

for f in "$bat" "$ps1" "$sh"; do
  if [ ! -f "$f" ]; then
    echo "ERROR: missing $f" >&2
    exit 1
  fi
done

targets=(
  "windows-x64    x86_64-pc-windows-gnu        runner.exe"
  "windows-arm64  aarch64-pc-windows-gnullvm   runner.exe"
  "linux-x64      x86_64-unknown-linux-gnu     runner"
  "linux-arm64    aarch64-unknown-linux-gnu     runner"
  "macos-x64      x86_64-apple-darwin          runner"
  "macos-arm64    aarch64-apple-darwin          runner"
)

echo "Building all targets..."
cd "$dir"
cargo zigbuild --release \
  --target x86_64-pc-windows-gnu \
  --target aarch64-pc-windows-gnullvm \
  --target x86_64-unknown-linux-gnu \
  --target aarch64-unknown-linux-gnu \
  --target x86_64-apple-darwin \
  --target aarch64-apple-darwin

echo "Assembling polyglot installer..."

{
  # Shebang and polyglot framing: batch/powershell/bash preamble
  echo '#!/usr/bin/env bash'
  echo ''
  echo 'echo \" <<'"'"'next'"'"' >/dev/null ">nul "\" \`" <#"'
  echo '@echo off & <nul set /p ="[6A[J"'
  # Batch body
  cat "$bat"
  echo 'goto :eof & type con >nul'
  # Transition from batch to PowerShell
  echo 'next'
  echo '#> | Out-Null'
  echo 'echo \" <<'"'"'next'"'"' >/dev/null # " | Out-Null'
  # PowerShell body
  cat "$ps1"
  echo 'while ( ! $MyInvocation.MyCommand.Source ) {'
  echo '    $input_line = Read-Host'
  echo '}; exit; <#'
  # Transition from PowerShell to bash
  echo 'next'
  echo 'set +o histexpand 2>/dev/null'
  # Bash body
  cat "$sh"
  echo 'case $- in *"i"*) cat /dev/stdin >/dev/null ;; esac'
  echo 'exit'
} > "$output"

# Append payloads
for entry in "${targets[@]}"; do
  read -r label rust_target binary <<< "$entry"
  bin="$dir/target/$rust_target/release/$binary"
  if [ ! -f "$bin" ]; then
    echo "ERROR: missing $bin" >&2
    exit 1
  fi
  size=$(wc -c < "$bin" | tr -d ' ')
  printf "  %-16s %s (%s bytes)\n" "$label" "$rust_target" "$size"
  echo "payload:$label" >> "$output"
  base64 < "$bin" | fold -w 8000 >> "$output"
done

echo '#>' >> "$output"
chmod +x "$output"
echo "Written to $output"
