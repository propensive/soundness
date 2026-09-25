# The Toolchain

Soundness code should be written, built, tested, explored and debugged with tools that share
its philosophy — total, honest, native-feeling — rather than through a Maven-era toolchain that
predates every idea the platform embodies. The end-state is a developer, human or agent, who
never leaves the ecosystem: flame for exploration, fury for building, fume for testing,
fluence for finding APIs, the exegesis LSP server inside any editor, a debugger that speaks
Soundness rather than Java, and the synesthesia MCP server giving agents the same access
programmatically.

The pieces are in different states, and the roadmap is honest about which. exegesis,
synesthesia and vivisection are working modules in this repository. flame, fume and fury are
separate projects with their own repositories and roadmaps: this track records only what
Soundness owes each of them, never their own progress. flame and fume are pinned as tools in
`etc/tools` (0.3.0 and 0.4.1) and configured under `.pyrocosm/`; fume runs this repository's
suites as a front-end and runner over the probably framework, which remains the library.
fluence is not started. TEL — the configuration language the whole toolchain standardises on —
is specified and implemented, with stratiform as its reference implementation. The bootstrap
test for the entire track is Soundness being buildable by fury, attestation-equal to the Mill
build.

## tool-1: flame tracks current releases

Horizon: near
Baseline: flame 0.3.0 is pinned in `etc/tools`; the current Soundness release is 0.68.0 (measured 2026-09-25; on 2026-08-01 flame built against 0.62.0 while the release was 0.64.0)

A REPL that lags the platform cannot be the platform's front door. flame builds against each
release as it happens, and staying current becomes a checked property rather than an intention.
Since #2044 flame is a pinned tool with its own `.pyrocosm/flame/config.tel`, and `make
doccheck` runs every tutorial's examples through it; nothing yet ties a Soundness release to a
green flame build.

Done when: flame's CI builds it against every new Soundness release, and a release is not
announced until that build is green.

## tool-2: fume runs the suites

Horizon: near → mid
Baseline: fume 0.4.1 runs the suite; 1 test suite (orthodoxy) is disabled (measured 2026-09-25; fume did not exist and 3 suites were disabled on 2026-08-01)

fume is the testing tool over the probably framework: multiple report formats, live updates in
the terminal, and results recorded as git notes alongside the existing attestation notes. Its
first milestone — running this repository's own suites in CI — is reached: `make ci` is
`fume run` over the umbrella assembly named in `.pyrocosm/fume/config.tel`, and probably no longer
renders reports of its own. What remains is the disabled suites.

Done when: fume runs the full Soundness suite in CI and

    grep -c 'enabled = false' build.mill    # 0

## tool-3: the LSP server serves a real session

Horizon: near → mid

exegesis provides the LSP framework; the criterion is lived experience made mechanical: a
scripted editor session — open, diagnose, complete, navigate — runs against a Soundness project
in CI. exegesis's own suite already drives an in-process server over JSON-RPC (initialize,
open, diagnose, hover, incremental change); completion, navigation and a real transport are
what separate it from the criterion.

Done when: the scripted editor-session test passes in CI.

## tool-4: Soundness builds with fury

Horizon: mid

The bootstrap is the criterion: `fury build` produces the same artifacts as the Mill build,
verified by the same attestation input-digest discipline, on this repository — the hardest
Scala build it will ever face. fury itself is an [external gate](index.md#dependencies); what
this item tracks is Soundness's side of the bootstrap — the things a build tool needs from this
repository's own libraries, each filed as an issue: a compiler-version accessor (#2026),
the diagnostics `ScalacEdges` drops (#2027), retained compiler sessions for a daemon (#2028),
TASTy UUIDs against LIRA §17 determinism (#2029), the `Materializer` cache against the store's
derivative tier (#2025), the manifest `source` record (#2022), section-scoped `Tool` records
and `Setting` (#2023), the closed `Lira.Hash.Domain` (#2024) and `TelBlueprint` nesting with
custom validators (#2030).

Done when: `fury build` builds Soundness from a clean checkout, attestation-equal to the Mill
build.

## tool-5: agents reach everything over MCP

Horizon: mid

synesthesia exposes the platform's knowledge to agents: module documentation, `SN-` error
pages, roadmap status from `status.tel`, and migration instructions. An agent should never
need to clone the repository to answer "what does SN-042 mean?" Today synesthesia is the
generic MCP library — tools, resources and prompts — and none of those resources exists.

Done when: a scripted MCP session resolves an `SN-` code to its page, a module to its topic
guide, and a roadmap item to its status, in CI.

## tool-6: the property-testing question is settled

Horizon: mid

probably tests over axes and spreads rather than `forAll` and shrinking. Either that is the
answer — in which case a philosophy page argues it — or generative testing with shrinking
ships in probably. Either artifact closes the item; what is not acceptable is the question
staying open by default.

Done when: probably ships `forAll`-style generation with shrinking, or a philosophy page
documents why axes and spreads are the design.

## tool-7: fluence searches the APIs

Horizon: long
Needs: doc-5

fluence makes the API surface searchable — by name, by type, by signature — over the extraction
pipeline that `doc-5` builds.

Done when: fluence answers name, type and signature queries against the current release's
published API documentation.

## tool-8: fume reports live to the web

Horizon: long

fume's second front-end: live test reporting over HTTP and WebSockets, the same data that
drives the terminal renderer.

Done when: a fume run can be watched live in a browser, and the recorded git-note report
matches what was streamed.

## tool-9: TEL everywhere

Horizon: long

Every ecosystem tool that needs configuration reads TEL, with stratiform as the reference
implementation. No tool in the ecosystem asks for YAML, JSON or HOCON configuration. The three
tools this repository runs — flair, flame and fume — already read theirs from
`.pyrocosm/<tool>/config.tel`.

Done when: no ecosystem tool's own configuration surface accepts any format but TEL.

## tool-10: a debugger that speaks Soundness

Horizon: near → mid
Baseline: vivisection ships a JDWP core, an expression evaluator and a DAP server; its suite drives a live session end to end; an object still inspects as its JVM class name (measured 2026-09-25)

Debugging a running application should feel native, not hosted: breakpoints, stepping and
inspection through a Debug Adapter Protocol server, with inspected values rendered as their
Soundness types — the same no-Java-encodings discipline `core-5` applies to traces, applied
to live state. vivisection is that server: `vivisection.DapServer` handles the request set
(launch, attach, breakpoints, threads, stack traces, scopes, variables, evaluate, stepping),
and its suite's `DapClient` drives a live debuggee through breakpoints and inspection in the
ordinary run. What separates it from the criterion is the rendering — an object snapshot
inspects as, say, `scala.collection.immutable.List`, which is exactly the encoding `core-5`
forbids — and a `doc/modules/` topic.

Done when: a scripted DAP session — set a breakpoint, step, inspect a value rendered in
Soundness terms — passes against a running Soundness application in CI.

## tool-11: coverage is tracked

Horizon: mid
Needs: tool-2

probably's own scoverage reader went with its report renderer when fume took over reporting;
coverage returns as a fume concern, a tracked property rather than a one-off report: unit-test
coverage recorded for every commit, stored in git notes alongside the test reports, so the
trend is queryable and a regression is visible at review time. flair's unsafety census
(`safety-7`) already keeps its record this way, and is the pattern to follow.

Done when: every CI run records coverage in a git note, and a single command reports the
coverage delta between any two commits.

## tool-12: benchmarks on every commit

Horizon: mid
Needs: tool-2

The benchmark suites run for every commit, with results stored in git notes under the same
discipline as test reports and attestations, so performance history travels with the
repository and regressions are caught when they land, not when users notice.

Done when: every commit on the main branch carries a benchmark git note, and a single command
reports the benchmark delta between any two commits.
