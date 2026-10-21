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
Baseline: 104 of 145 rows build for JavaScript, 108 for Native; 34 JavaScript exclusions, every one with a reason (measured 2026-10-21; 101 of 144 on 2026-09-25; 94 of 132 on 2026-08-01)

`doc/compatibility.tsv` records the matrix, and `etc/check-compatibility.py` (run by `make
build`) regenerates its platform columns from `build.mill` — each component's `scalaJs`/
`scalaNative` flags and `sources`/`jsSources`/`nativeSources` source sets — and fails on any
divergence from the committed copy or on an exclusion with no reason in `notes`. The matrix is
therefore a contract: a module added without a row, or a flag flipped without the file, fails
the build, and the only hand-written content is the reason for every `no` or `core` cell.

Done when: the ordinary build regenerates the manifest, fails on divergence from the committed
copy, and no exclusion row has an empty reason — met by `etc/check-compatibility.py`. Interim gauge:

    grep -v '^#' doc/compatibility.tsv | awk -F'\t' '$3=="yes"{n++} END{print n}'    # 104 → 145 minus inherent exclusions

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

Known non-inherent Native gaps, each a candidate leg: guillotine's pseudo-terminal (`Command#pty`
raises on Native until it is rewritten over posixlib's `posix_openpt`; the JVM one is Panama);
coaxial's multicast (`joinMulticast` raises, which keeps syndesis off); hellenism's `servicesFor`
(`URLClassLoader`); and a native `Http.Backend` for telekinesis, whose model now cross-compiles.

The link check itself needs reviving before it can be widened: `mill nativelink.testSuites` is
outside `make attest`, two of its three enrolled suites (denominative, monotonous) no longer
compile natively, and `nativelink.binary` itself — like any binary reaching
`contingency.ThrowTactic` — fails to link because the Native scalalib has no
`scala.caps.SharedUnscoped` (measured 2026-10-21, while enrolling imperial).

## plat-4: WASI beyond the first seven backends

Horizon: mid → long
Baseline: 7 WASI backends; HTTP sends any method with a body, sockets are TCP-only (measured 2026-09-25; HTTP was GET-only on 2026-08-01)

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
Wasm OCI Artifact, anthology's `OciImage` format reaches that from a compilation in one path, and
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
