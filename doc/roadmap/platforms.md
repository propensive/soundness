# Platform Parity

A batteries-included platform earns the name only if the batteries work where applications
actually run: on the JVM, in browsers, as native binaries, inside WASM runtimes, and on Android
devices. The promise is not that every module runs everywhere — a browser has no filesystem —
but that every exclusion is inherent, stated, and checked, and that building for any target is
no harder than building for the JVM.

Today the JVM is total, JavaScript and Native cover most modules, WASI is a narrow but real
slice built on the WASM component model, and Android works end-to-end — D8 dexing and APK
signing without the Android SDK — but outside the ordinary build. Parity means closing each of
those qualifications, and the instrument for doing so is a manifest: the platform matrix as
checked-in data, where every "no" carries a reason, and CI fails when reality regresses from
the manifest. When the only reasons left are inherent ones, the parity statement holds.

## plat-1: the platform manifest is enforced

Horizon: near
Baseline: 94 of 132 modules build for JavaScript, 86 for Native (measured 2026-08-01)

`doc/compatibility.tsv` already records the matrix; enforcement makes it a contract rather
than a report: CI regenerates it and fails on any regression, and every exclusion carries a
stated reason.

Done when: the ordinary build regenerates the manifest, fails on divergence from the committed
copy, and no exclusion row has an empty reason. Interim gauge:

    grep -v '^#' doc/compatibility.tsv | awk -F'\t' '$3=="yes"{n++} END{print n}'    # 94 → 132 minus inherent exclusions

## plat-2: Android in the ordinary build

Horizon: near → mid

The dexing and APK-signing path works but is gated on `ANDROID_HOME` and excluded from CI. An
Android application should be built and signed on every attested run, so regressions surface
immediately rather than on the next manual attempt.

Done when: `make attest` includes the Android stage, and CI records a signed APK for every
release.

## plat-3: JavaScript and Native at full non-excluded coverage

Horizon: mid
Needs: plat-1

Every module builds for JavaScript and Native unless its exclusion reason is inherent, and
Native coverage is verified by the real binary link check throughout, not compilation alone.

Done when: the manifest shows no JavaScript or Native exclusion whose reason is not inherent,
and the Native link check covers every Native-capable module.

## plat-4: WASI beyond the first seven backends

Horizon: mid → long
Baseline: 7 WASI backends; HTTP is GET-only, sockets are TCP-only (measured 2026-08-01)

The WASI slice — environment, clock, random, sockets, filesystem, HTTP, stdio — proves the WIT
component-model approach; parity requires the full HTTP method set, UDP, and backends for every
module whose domain WASI can express. The compiler side of this is within reach: Soundness is
built with Proscala, so the Wasm backend and the binding generation (today an out-of-tree
`wit-bindgen` fork, to be vendored) are ours to fix. The genuinely external gate is WASI
itself: its interfaces must mature far enough to express what the backends need.

Done when: the manifest's WASI column matches the JavaScript column except where the exclusion
reason names a missing WASI capability. Interim gauge:

    ls -d lib/*/src/wasi | wc -l    # 7 and rising

A component is now deployable as well as runnable: `Image.wasm` (embarcadero) packages one as a
Wasm OCI Artifact, `Artifact.OciImage` (anthology) links straight to that from a compilation, and
`make wasm-e2e` serves the packaged HTTP component under `wasmtime serve`. What is still missing
between here and a Wasm workload running on a cluster is distribution — nothing in the tree speaks
the OCI registry protocol, so an artifact reaches a registry only via `skopeo` or `ctr` — and the
containerd client cannot yet name a Wasm runtime when it creates a container.

## plat-5: the parity statement holds

Horizon: long
Needs: plat-4

The end-state, stated as a fact about the manifest: no exclusion reason anywhere in it means
"not yet". Every reason is inherent — the platform cannot support the capability — and the
manifest's reasons are themselves the completion signal.

Done when: every exclusion reason in `doc/compatibility.tsv` describes an inherent platform
limitation, and a reason vocabulary check in CI enforces the distinction.

## plat-6: the browser is a first-class application target

Horizon: mid

Compiling for JavaScript is necessary but not sufficient: a website built with Soundness
should be written in Soundness end-to-end, with the front-end in Scala.js and every web API —
DOM, fetch, storage, media — reached through typed facades generated from WebIDL by
xenophile, over querencia's typed DOM. No hand-written JavaScript, no untyped `js.Dynamic`
seams. The proof is dogfood: soundness.dev's own front-end, served by the in-tree website,
built this way.

Done when: the soundness.dev front-end ships as Soundness-compiled Scala.js, with all web API
access through generated typed facades, and builds in the ordinary build.
