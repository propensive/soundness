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
script="$(realpath "$0")"
output="${script%.*}"
[ "$output" = "$script" ] && output="${script}.bin"
indexline=$(grep -n "^index:" "$script" | head -1)
indexnum=${indexline%%:*}
indexcontent=${indexline#*index:}
get_offset() {
  printf '%s\n' "$indexcontent" | tr ',' '\n' \
    | grep "^$1=" | cut -d= -f2
}
decode() { base64 -d 2>/dev/null || base64 -D; }
extract() {
  local off=$1 absline
  absline=$((indexnum + off + 1))
  tail -n +"$absline" "$script" \
    | sed -n '/^-----END/q; p' \
    | decode
}
offset=$(get_offset "${os}-${arch}")
if [ -z "$offset" ]; then
  printf "No payload for %s-%s\n" "$os" "$arch" >&2
  exit 1
fi
extract "$offset" > "$output"
data_offset=$(get_offset "data")
if [ -n "$data_offset" ]; then
  extract "$data_offset" >> "$output"
fi
chmod +x "$output"
printf "Extracted to %s\n" "$output"
