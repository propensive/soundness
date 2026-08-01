# The Roadmap

Soundness is being built towards a single destination: the point at which a serious team can
adopt it for real applications. Not a version number, and not a ceremony — a set of measurable
conditions, listed at the bottom of this page as [the gate](#the-gate). This roadmap describes
the route: eight parallel tracks, each with its own end-state, phased across three horizons.

## The vision

Everything on this roadmap serves one philosophy, elaborated in the
[philosophy](../philosophy/composability.md) pages:

- **Safety.** It is difficult or impossible to write Soundness code that fails surprisingly at
  runtime. Every API is total, or [declares the errors it raises](../philosophy/error-handling.md).
  Mutability is permitted only where separation checking proves it safe; capture checking
  enforces what signatures promise. Partiality anywhere — including in the Java and Scala
  standard libraries beneath us — undermines everything, so partial APIs are replaced, not
  tolerated.
- **Coherence.** APIs are predictable, clear and generalised. Similar things share one API;
  variation is a parameter, not a name. If two things share a name, they are the same thing; if
  they have different names, they are not.
- **Batteries included.** One platform for CLI applications, web applications (server and
  front-end), testing, scientific work and development tooling — built on
  [open standards](../tags.md), not proprietary inventions, and modular enough that unused
  batteries cost nothing.
- **Parity of targets.** JVM, Android, JavaScript and WASM/WASI are all viable targets, no
  harder to build for than the JVM.
- **A native-feeling world.** Developing, debugging and running Soundness code never feels like
  operating an advanced system inside an environment that wasn't designed for it. Java encodings
  of values are never visible.
- **Learnable, documented, specified.** Every API is documented; every error message is
  informative and has a documented page; every novel idea has a specification. Documentation is
  written so that both people and AI agents can act on it reliably.
- **Migration-first stability.** APIs change whenever a better or safer design exists. Stability
  comes not from freezing APIs but from making migration cheap: every breaking change ships with
  instructions precise enough for an agent to execute.
- **A trusted toolchain.** Building, testing, debugging, exploring and distributing Soundness
  code happens through its own tools — `fury`, `fume`, `flame`, `fluence`, the `exegesis` LSP
  server, a Soundness-native debugger and the `synesthesia` MCP server — with releases that are
  attested, predictable and verifiable, and libraries distributed as LIRA files that are
  guaranteed to compose safely. TEL is the configuration language wherever configuration is
  needed. Soundness is built with the Proscala compiler, which is modifiable whenever safety
  demands it, under one constraint: published artifacts stay readable from the mainline Scala
  compiler.

## How to read this roadmap

Each track lives in its own file and follows the same shape: a short preamble saying why its
end-state matters, then a sequence of numbered items. Every item uses one format:

```
## core-3: no direct `scala.collection` imports

Horizon: near → mid
Needs: core-1
Baseline: 219 files (measured 2026-08-01)

One or two sentences of prose.

Done when:

    git grep -l 'import scala.collection' -- lib | grep -v '^lib/proscenium/' | wc -l    # 0
```

The rules that keep this document trustworthy:

- **Item identifiers are permanent.** They are track-prefixed and sequential, and are never
  reused — the same discipline as `SN-` error codes. A completed item keeps its heading, gaining
  a `Done:` line naming the closing commit or pull request. The only exception is an item whose
  criterion *is* the deletion of an artifact.
- **Every criterion is a command or an observable fact.** A `Done when:` block contains a
  runnable command with its expected output as a trailing comment, or states a fact about a
  file's existence, emptiness, or a CI check's status. Nothing else is admissible. "Mostly
  complete" and "substantially done" are not criteria; if it cannot be counted, grepped or
  observed, it does not belong here.
- **Baselines are dated.** Numbers drift as work proceeds; an agent re-measuring a baseline
  should expect drift and report it, not silently trust a stale figure.
- **Horizons are not dates.** *Near* is the current working set; *mid* is what the near horizon
  unblocks; *long* approaches the track's end-state. Progress is measured by criteria, never by
  the calendar.
- **Status lives in [`status.tel`](status.tel).** The prose files say what and why;
  `status.tel` — one record per item, machine-readable, written in TEL — says where each item
  stands. Prose never claims completion; it links.

## The tracks

1. **[The standard-library drain](core.md)** (`core`) — no module reaches the Java or Scala
   standard libraries except through proscenium's opaque boundary; nothing Java-shaped is
   visible at development or debug time.
2. **[Capabilities and effects](safety.md)** (`safety`) — the `caps.unsafe` escape hatches reach
   zero; capture checking, separation checking and declared errors hold without exemption; the
   compiler fork is sustainable.
3. **[Platform parity](platforms.md)** (`plat`) — building for JavaScript, Native, WASI and
   Android is no harder than for the JVM, with only inherent exclusions, verified in CI; the
   browser is a first-class application target, with web APIs behind typed facades.
4. **[API coherence](api.md)** (`api`) — one name means one thing; variations are parameters;
   the exported surface is deliberate rather than accidental.
5. **[Documentation and learnability](documentation.md)** (`doc`) — one documentation system,
   complete coverage, published API docs, and a path from nothing to a working application that
   an agent can follow.
6. **[The toolchain](tooling.md)** (`tool`) — flame, fury, fume, fluence, the LSP server, the
   debugger and the MCP server together replace every Maven-era tool, with coverage and
   benchmarks tracked for every commit in git notes.
7. **[Distribution and release](distribution.md)** (`dist`) — attested, changelogged releases;
   the migration-instruction convention; LIRA from specification to sole channel, retiring
   Maven Central.
8. **[Standards breadth](breadth.md)** (`brd`) — the remaining standards a production team
   expects: JOSE, TOML, WebAuthn, QUIC, IMAP.

## Dependencies

An edge `a ← b` means *a cannot be completed before b* (it may be started). An item's `Needs:`
line records its hard dependencies, including those within its own track; the list below
duplicates only the cross-track edges, because those are the ones requiring coordination. If
this list ever grows much beyond a dozen edges, the tracks are wrongly factored, and
refactoring them is the fix.

- `api-6 ← dist-2` — surface changes flow through the migration convention once it exists
- `doc-8 ← tool-2, tool-4` — the walkthrough uses fume and fury
- `tool-7 ← doc-5` — fluence consumes the API-extraction pipeline
- `dist-4 ← doc-5` — API-derived versioning needs the same extraction
- `dist-5 ← tool-4` — fury must build before it can publish

**External gates** — conditions that gate items but are not under this project's control. They
are listed here and never appear as items, because no `Done when:` command of ours can close
them:

- WASI's standard interfaces mature far enough to express full HTTP and UDP (gates `plat-4`).

The compiler is deliberately *not* on this list: Soundness is built with Proscala, and the
freedom to modify it — for capture-checking fixes, for the Wasm backend, for whatever safety
demands — is assumed throughout this roadmap, bounded only by the requirement that published
artifacts remain readable from mainline Scala (`safety-5`).

## The gate

Production-readiness is the conjunction of the following criteria. The gate defines nothing
new: each entry cites a track item, and the item's `Done when:` command is the authority.

1. `proscenium.compat` is empty (`core-2`).
2. No file outside proscenium imports `scala.collection` (`core-3`).
3. No `caps.unsafe` escape hatch remains in `lib/` (`safety-4`).
4. The platform manifest is enforced in CI, its exclusions carry reasons, and an Android APK is
   built and signed in CI (`plat-1`, `plat-2`, `plat-3`).
5. `doc/api-reduction-candidates.md` no longer exists (`api-5`).
6. Every error type has an `SN-` code and a documentation page, checked in CI (`doc-1`).
7. Every published module is covered by a `doc/modules/` topic (`doc-4`).
8. API documentation is generated and published for every component (`doc-5`).
9. `fury build` builds Soundness itself, attestation-equal to the Mill build (`tool-4`).
10. fume runs the full test suite, and no suite is disabled (`tool-2`).
11. A scripted LSP editor session passes in CI (`tool-3`).
12. Every breaking change in the last ten consecutive releases shipped with agent-executable
    migration instructions (`dist-2`).
13. Every release is published as attested LIRA files (`dist-7`).
14. JOSE, TOML and WebAuthn are shipped (`brd-1`, `brd-2`, `brd-3`).

And one integrative criterion, which is the point of the exercise:

15. An agent, following only the documentation in `doc/`, takes a new project from nothing to a
    tested, deployed JVM-and-JavaScript web application using only ecosystem tools — and this
    walkthrough is executed and recorded in CI (`doc-8`).

### Beyond the gate

The gate is not the horizon. Past it lie: Maven Central publishing retired, leaving LIRA as
the sole channel (`dist-8`); QUIC/HTTP-3 and IMAP (`brd-4`, `brd-5`); WASI at full parity once
its external gate lifts (`plat-4`, `plat-5`); and the fork's mainline-readability guarantee
enforced in CI (`safety-5`).

## For AI agents

This document is written to be acted on by agents as much as read by people:

- Item identifiers are stable anchors; cite them (`core-3`, `dist-2`) rather than describing
  work in prose.
- Verify any claim of progress by running the item's `Done when:` command; never trust prose or
  memory over the command's output. Re-measure baselines before relying on them and report
  drift.
- Read [`status.tel`](status.tel) for current status; update it in the same commit as the work
  it describes.
- When a roadmap item changes a public API, follow the migration-instruction convention defined
  by `dist-2` in `doc/standards/migration.md` — that convention is what makes "APIs change
  freely" and "code keeps working" compatible.
