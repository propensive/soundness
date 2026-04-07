os=$(uname -s)
arch=$(uname -m)
printf "Operating system: %s\nArchitecture: %s\n" \
  "$os" "$arch"
case "$os" in
  Darwin) os=macos ;; Linux) os=linux ;;
  *) printf "Unsupported OS: %s\n" "$os" >&2; exit 1 ;;
esac
case "$arch" in
  x86_64)          arch=x64 ;;
  aarch64|arm64)   arch=arm64 ;;
  *) printf "Unsupported arch: %s\n" "$arch" >&2; exit 1 ;;
esac
label="payload:${os}-${arch}"
script="$(realpath "$0")"
output="${script%.*}"
[ "$output" = "$script" ] && output="${script}.bin"
start=$(grep -n "^${label}$" "$script" \
  | head -1 | cut -d: -f1)
if [ -z "$start" ]; then
  printf "No payload for %s\n" "$label" >&2
  exit 1
fi
decode() { base64 -d 2>/dev/null || base64 -D; }
tail -n +"$((start + 1))" "$script" \
  | sed -n '/^payload:/q; p' \
  | decode > "$output"
chmod +x "$output"
printf "Extracted to %s\n" "$output"
