### Signing a Release

Ethereal applications can be shipped with a baked-in ML-DSA-44 public key
that the launcher uses to verify any candidate upgrade dropped at
`{XDG_DATA_HOME}/{app-name}/.pending` before swapping it into place.
Verification happens in the Rust launcher wrapper — before the JVM starts —
so the trust boundary is not crossed by any Scala code in the running
application.

This document walks through the one-time key setup and the per-release
signing workflow.

#### One-time: generate a signing keypair

The `ethereal-sign` CLI lives alongside the launcher under
`lib/ethereal/src/sign/`. Build it once:

```sh
cargo build --release --bin ethereal-sign --features sign \
            --manifest-path lib/ethereal/Cargo.toml
```

Generate a keypair:

```sh
./out/rust/release/ethereal-sign keygen --out release-keys/myapp
```

This produces two files:

- `release-keys/myapp.seed` — 32 bytes of FIPS-204 signing-key seed.
  **Keep this offline.** Anyone with this seed can ship a binary that
  end-users' launchers will accept as a legitimate upgrade.
- `release-keys/myapp.pub` — 1312 bytes of ML-DSA-44 public key. This is
  what gets baked into the published binary.

Treat the seed like any other release-signing key — store it on offline
media or in an HSM, never in a CI variable, and rotate it deliberately.
**Lost or compromised seeds cannot be revoked at the launcher level**: the
public key is baked into every shipped binary, and only a fresh release
with a new public key updates that. Plan accordingly.

#### Per-release: build a signed binary

A signed release is produced in two steps. First, build the application
binary with the public key baked into its ETHRCFG block:

```sh
java -Dbuild.executable=dist/myapp \
     -Dbuild.id=42 \
     -Dethereal.publicKey=release-keys/myapp.pub \
     -jar dist/myapp.jar '[]'
```

The relevant arguments:

- `-Dbuild.executable=<path>` — output path for the runner+JAR file. Same
  as any unsigned Ethereal build.
- `-Dbuild.id=<u64>` — monotonically-increasing build identifier. The
  signed-upgrade verifier rejects downgrades by comparing this against
  the running launcher's own `build_id`.
- `-Dethereal.publicKey=<path>` — the 1312-byte raw public-key file
  produced by `ethereal-sign keygen`. **Without this property the build
  emits a binary whose launcher will reject every upgrade** (the safe
  default for `make install`–style local builds where the upgrade path
  is never exercised).

The output at `dist/myapp` is a normal executable, but its signature slot
is all zero. Sign it in place to a new path:

```sh
./out/rust/release/ethereal-sign sign \
    --key release-keys/myapp.seed \
    --in  dist/myapp \
    --out dist/myapp.signed
```

`dist/myapp.signed` is byte-for-byte the file your distribution channel
should hand to end users — it's both a valid executable and a valid
`.pending` for the launcher to apply on a future upgrade.

#### Per-release: signing a downgrade

If a release needs to be installable over a newer build (e.g. you've
shipped 43 and discovered it's broken; you want users still on 42 to
*not* upgrade to 43, and users already on 43 to roll back to 42), sign
the rollback with `--allow-downgrade`:

```sh
./out/rust/release/ethereal-sign sign \
    --key release-keys/myapp.seed \
    --in  dist/myapp \
    --out dist/myapp.signed \
    --allow-downgrade
```

The flag lives in a byte inside the signed payload, so an attacker cannot
flip it on a binary you signed without it. The launcher honours it only
when the signature verifies.

#### What users see

End users install your application however they normally would. When the
application later receives a signed upgrade — typically via the
application's own `Upgrade(source)` call, where `source` could be a URL,
a file path, or any other `Readable by Bytes`:

```scala
Upgrade(url"https://releases.example.com/myapp.signed")
```

— the launcher writes the bytes to `.pending`, spawns a fresh launcher,
and exits. The fresh launcher reads `.pending`, verifies the ML-DSA-44
signature against the public key it has baked in, checks the embedded
`build_id` against its own, and either swaps the new binary into place
(re-execing into it) or silently deletes `.pending` and continues running
the existing binary. The runner's debug-trace output explains rejections
when `ETHEREAL_DEBUG=1` is set.

#### What's actually signed

The launcher's verifier zeroes the 2420-byte signature slot in a working
copy of the candidate binary and verifies the FIPS-204 ML-DSA-44
signature over the result. The signed payload therefore covers:

- the launcher's executable code (any tampering invalidates the
  signature),
- the entire bundled JAR appended to it,
- the embedded `build_id` (defeats replay of an older legitimately-signed
  release),
- the `flags` byte that controls per-upgrade downgrade permission
  (attackers cannot opt into downgrades on a binary you signed without
  the flag),
- the baked-in public key itself (defeats substituting a different
  release key into an otherwise-legitimate binary).

The FIPS-204 `ctx` parameter is empty, per the COSE algorithm registration
in RFC 9964.

#### Key rotation

There is no in-launcher mechanism to switch keys. To rotate:

1. Generate a new keypair as above.
2. Ship one final release signed with the **old** seed whose embedded
   public key is the **new** one. Users on existing installs apply this
   release through the normal signed-upgrade path, and from that point
   on their launchers trust only the new key.
3. Subsequent releases are signed with the new seed.

Skipping the bridging release strands existing installs — they hold the
old public key and will reject everything you sign with the new seed.
