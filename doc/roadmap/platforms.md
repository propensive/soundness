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

No known non-inherent Native gap remains. Closed on 2026-10-22: guillotine's pseudo-terminal,
rewritten over Scala Native's C interop (`posix_openpt`, `posix_spawn`, `poll`; the JVM one is
Panama), so `Command#pty` runs real terminals on Native. Closed on
2026-10-21: coaxial's native backend joins multicast groups (bringing syndesis to Native);
telekinesis's `soundnessHttp` client moved into the cross-compiling `telekinesis.sockets`, giving
Native an `Http.Backend` (HTTP/1.1, over coaxial's OpenSSL connection for `https`); and
hellenism's classloader construction moved into a `core-jvm` source set, so `cp"…"` resources,
cataclysm's HTML stylesheets and the seven WASI backends compile for Native too.

Cryptography is no longer a JavaScript exclusion: enigmatic's pure-Scala `SoundnessCrypto`
(AES in every mode, HMAC, AES-GCM, ChaCha20-Poly1305, X25519, Ed25519) backs the Scala.js and
Scala Native builds, and `import providers.soundnessProvider` selects it anywhere. What it does
not cover — RSA, DSA, ECDSA, ML-DSA — stays with the JDK and OpenSSL. A provider over the
browser's Web Crypto API is gated on JSPI: `crypto.subtle` is Promise-only and the `Crypto`
contract is synchronous, so it needs the fork's `js.async`/`js.await` proven under the Wasm
backend first (plat-6); with that, Web Crypto slots in as an alternative provider for the
primitives it has, without an API change.

The link check links again (2026-10-22): Proscala 1.0.4 publishes its supplementary library as
`proscala-library_native0.5_3`, so `scala.caps.SharedUnscoped` (which every Native link then
reached through `contingency.ThrowTactic`; since #2328 only `scala.Spreadable` is reached) has
NIR from the toolchain itself, through
`nativeToolchain.runtimeDeps`; the one-trait `nativelink.shim` that bridged the gap is gone. The
same release carries `tasty-core` for Scala.js and Scala Native, which is what lets
`hyperbole.tasty` read TASTy on both without the compiler. `nativelink.binary` runs on every
attest, exercising the FFI, filesystem, sockets, supervisors, the pseudo-terminal and OpenSSL;
widening it to every Native-capable module (and reviving `nativelink.testSuites`) remains.

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

## plat-7: degustation atomizes TASTy without the compiler

Horizon: mid
Needs: plat-3
Baseline: `degustation.core` (1,179 lines) drives `scala3-tasty-inspector` over the compiler's reflection; JVM-only, with `scala3-compiler_3` on its classpath (measured 2026-10-23)

degustation — the `tasty/1` atomizer behind LIRA's Scala discipline — is the last module
kept off JavaScript and Native by the compiler for a reason that is not inherent: the
discipline is *defined* over the compiler's semantic model (`tasty.md` §4), so the atomizer
pattern-matches `quotes.reflect` types and asks symbols for their `fullName`, `signature`,
`children` and flags. Proscala 1.0.4 publishes `tasty-core` for all three platforms, and
`hyperbole.tasty` already reads TASTy through it, so the question is what degustation needs
beyond the bytes. Scoped on 2026-10-23:

- **Structural, and free from raw TASTy**: modifiers (`tasty/1`'s whole flag set,
  `PRIVATEqualified` for the visibility rule, `ARTIFACT`/`SYNTHETIC`), annotations (including
  the `@Child` annotations that are the sealed/enum child list, in declaration order), a
  template's parents, self type, type parameters and declarations, every member's declared
  type (parameter and result `tpt`s, bounds, aliases), inline and macro bodies as trees with
  local symbols addressable for alpha-normalization, package-qualified references
  (`TERMREFpkg`/`TYPEREFpkg`), and — on the *referencing* side — the erased signature itself,
  which every `TERMREF` to an overloaded member carries in its `SIGNED` name. The compiler's
  `LazyRef` placeholders, which §7 has to special-case, do not exist in the file.
- **Semantic, and needing a model of the dependency classpath**: two things only. (1) The
  erased-signature disambiguator of a *declaration*'s key (§6): erasing a declared parameter
  type means dealiasing it, erasing an opaque type and a value class to their underlying
  types, a type-parameter reference to its bound, and an intersection by the class/trait
  distinction of its parts — each of which asks what a referenced type *is*, in a dependency's
  TASTy or a Java classfile. (2) The declaring owner of a selected member (§6 "keying is by
  declaration", `TastyDiscipline`): `foo.bar` pickles as `bar`'s signed name against `foo`'s
  type, and finding that `bar` is declared in `Base` is a member lookup along `Foo`'s parents.
  Neither needs subtyping, type inference, as-seen-from substitution or completion order; both
  need the classpath loaded lazily, which is what `tasty-query` exists to do.

Three ways to close it:

1. **Own semantic layer over `tasty-core`** (recommended): a tree unpickler into plain data
   (~2,000 lines; `hyperbole.tasty` contributes the header and name table), a lazy classpath
   loader over directories and jars with a Java classfile reader for class kind, type
   parameters and member descriptors (~1,200), Scala 3 erasure and signature spelling (~900,
   after dotc's `TypeErasure` and `Signature`), member lookup along parents (~300), and the
   atomizer re-expressed over the data model (~1,200, the encoding rules unchanged). Roughly
   6,000 lines against `tasty-query`'s 19,000, because degustation needs none of its
   subtyping, type maps or Scala 2 pickle reader.
2. **Depend on `tasty-query`**: ~1,000 lines changed, but a 19,000-line dependency published
   for the JVM and Scala.js only — Native would need a cross-build published by us, as
   Proscala now does for `tasty-core` — whose erasure and member resolution are its own
   reimplementation of the compiler's, with the same coupling to each Scala release.
3. **Leave it on the compiler** and classify the exclusion as inherent: atomization runs at
   assembly and publish time, where a JVM is at hand. Honest today, but it rules out
   atomizing from a Native or Wasm build tool, which is the reason to want it.

Either of the first two is a new discipline, `tasty/2` (`tasty.md` §1: any change to the
canonicalization is a new discipline): §4 extraction over raw TASTy instead of the compiler's
model; §6, §11 and §12 spelling erasure as the discipline defines it rather than "as the
compiler computes it"; §7 losing the `LazyRef` clause; §14's promise becoming checkable in
fact, since two toolchains can no longer disagree about a model only one of them builds. The
verification is differential: run the new atomizer and the compiler-driven one over every
`.tasty` Soundness publishes and demand identical (key, class, value) listings — the existing
"a real module's TASTy atomizes" tests are the seed of that corpus — before the compiler-driven
one is retired.

Progress (2026-10-23, branch `degustation-tasty`): option 1 built. `degustation.tasty` holds the
data model, the reader, the classpath `Universe`, `Erasure` and `Degustation.atomize`, and
crosses to all three platforms; `degustation.core` keeps `Inspection.atomize` as its filesystem
face (directories and jars through `java.util.zip`, the JDK's classes through the platform
class loader) and is JVM-only for that reason, with no compiler dependency. Over the 147 build
modules the atoms are byte-identical to the compiler-driven atomizer's except where the
compiler's output was an artifact of the run (`tasty.md` §16); the compiler-driven atomizer is
the test suite's oracle. `tasty.md` describes `tasty/2`.

Done when: `degustation.core` depends on `tasty-core` and not on the compiler, builds for
JavaScript and Native (or its filesystem face is shown to be the only JVM residue, and a
Native build tool can supply the JDK's classes as a jar), and atomizes the Soundness corpus
identically to the compiler-driven atomizer it replaces; `tasty.md` describes `tasty/2`.

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
