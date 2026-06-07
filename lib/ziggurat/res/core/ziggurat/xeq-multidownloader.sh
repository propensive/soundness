os=$(uname -s)
arch=$(uname -m)

case "$os" in
  Darwin) os=macos ;; Linux) os=linux ;;
  *)      printf "Unsupported OS: %s\n" "$os" >&2; exit 1 ;;
esac

case "$arch" in
  x86_64)         arch=x64 ;;
  aarch64|arm64)  arch=arm64 ;;
  *)              printf "Unsupported architecture: %s\n" "$arch" >&2; exit 1 ;;
esac

label="${os}-${arch}"
s=$(realpath "$0")
row=$(sed -n 's/^assets://p' "$s" | head -1 | tr ',' '\n' | grep "^${label}=" | head -1)
if [ -z "$row" ]
then printf "No binary for %s\n" "$label" >&2; exit 1
fi
value=${row#*=}
url=${value%%|*}
hash=${value#*|}
t="$s.tmp"
xeq_msg 33 ████████ 0 "Downloading…"
if command -v curl >/dev/null 2>&1
then curl -fsSL "$url" -o "$t" || exit 1
elif command -v wget >/dev/null 2>&1
then wget -qO "$t" "$url" || exit 1
else printf 'Need curl or wget\n' >&2; exit 1
fi
size=$(wc -c < "$t" | tr -d ' ')
xeq_msg 32 ████████ 1 "Downloaded $size bytes"
xeq_msg 33 ████████ 0 "Verifying SHA-256…"
g=$( { sha256sum "$t" 2>/dev/null || shasum -a 256 "$t"; } | cut -d' ' -f1)
if [ "$g" != "$hash" ]
then printf 'Hash mismatch\n' >&2; rm -f "$t"; exit 1
fi
xeq_msg 32 ████████ 1 "Verified SHA-256"
chmod +x "$t"
mv "$t" "$s"
exec "$s" "$@"
