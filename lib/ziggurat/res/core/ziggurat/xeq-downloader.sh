s=$(realpath "$0")
url=$(sed -n 's/^# URL=//p' "$s" | head -1)
hash=$(sed -n 's/^# HASH=//p' "$s" | head -1)
t="$s.tmp"
xeq_msg 33 ▅▅ 0 "Downloading…"
if command -v curl >/dev/null 2>&1
then curl -fsSL "$url" -o "$t" || exit 1
elif command -v wget >/dev/null 2>&1
then wget -qO "$t" "$url" || exit 1
else printf 'Need curl or wget\n' >&2; exit 1
fi
size=$(wc -c < "$t" | tr -d ' ')
xeq_msg 32 ██ 1 "Downloaded $size bytes"
xeq_msg 33 ▅▅ 0 "Verifying SHA-256…"
g=$( { sha256sum "$t" 2>/dev/null || shasum -a 256 "$t"; } | cut -d' ' -f1)
if [ "$g" != "$hash" ]
then printf 'Hash mismatch\n' >&2; rm -f "$t"; exit 1
fi
xeq_msg 32 ██ 1 "Verified SHA-256"
chmod +x "$t"
mv "$t" "$s"
exec "$s" "$@"
