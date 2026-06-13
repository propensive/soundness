# Runner stub manifests

Each `<version>.tsv` records the SHA-256 hashes of the reusable native runner stubs published
as the GitHub release `runners-<version>` (assets `runner-<label>[.exe]`). It is generated and
committed by `etc/ci/runners-release.sh` (`make runners-release RUNNERS_VERSION=<version>`).

Format — one tab-separated line per platform, sorted by label:

```
<label>	<sha256>
```

where `<label>` is one of `linux-x64`, `linux-arm64`, `macos-x64`, `macos-arm64`, `windows-x64`.

These hashes are the source of truth for application packaging: an online polyglot launcher
embeds them to verify the stub it downloads at runtime; a monoglot or offline build verifies
the stub bytes it downloads at build time against them. The stubs are version-independent and
reusable across applications — they are republished only when the Rust runner source under
`lib/ethereal` changes.
