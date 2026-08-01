# The Toolchain

Soundness code should be written, built, tested, explored and debugged with tools that share
its philosophy — total, honest, native-feeling — rather than through a Maven-era toolchain that
predates every idea the platform embodies. The end-state is a developer, human or agent, who
never leaves the ecosystem: flame for exploration, fury for building, fume for testing,
fluence for finding APIs, the exegesis LSP server inside any editor, and the synesthesia MCP
server giving agents the same access programmatically.

The pieces are in different states, and the roadmap is honest about which. exegesis and
synesthesia are working modules in this repository. flame exists as a separate, active project
pinned to an older Soundness. fume is scoped but unwritten: a front-end and runner over the
probably framework, which remains the library. fury and fluence do not exist as code. TEL — the
configuration language the whole toolchain standardises on — is specified and implemented, with
stratiform as its reference implementation. The bootstrap test for the entire track is fury
building Soundness itself, attestation-equal to the Mill build it replaces.

## tool-1: flame tracks current releases

Horizon: near
Baseline: flame builds against Soundness 0.62.0; the current release is 0.64.0 (measured 2026-08-01)

A REPL that lags the platform cannot be the platform's front door. flame builds against each
release as it happens, and staying current becomes a checked property rather than an intention.

Done when: flame's CI builds it against every new Soundness release, and a release is not
announced until that build is green.

## tool-2: fume runs the suites

Horizon: near → mid
Baseline: fume does not exist; 3 test suites are disabled (measured 2026-08-01)

fume is the testing tool over the probably framework: multiple report formats, live updates in
the terminal, and results recorded as git notes alongside the existing attestation notes. Its
first milestone is running this repository's own suites — all of them, including the three
currently disabled.

Done when: fume runs the full Soundness suite in CI and

    grep -c 'enabled = false' build.mill    # 0

## tool-3: the LSP server serves a real session

Horizon: near → mid

exegesis provides the LSP framework; the criterion is lived experience made mechanical: a
scripted editor session — open, diagnose, complete, navigate — runs against a Soundness project
in CI.

Done when: the scripted editor-session test passes in CI.

## tool-4: fury builds Soundness

Horizon: mid

The build tool is rebuilt, and the bootstrap is the criterion: `fury build` produces the same
artifacts as the Mill build, verified by the same attestation input-digest discipline, on this
repository — the hardest Scala build it will ever face.

Done when: `fury build` builds Soundness from a clean checkout, attestation-equal to the Mill
build.

## tool-5: agents reach everything over MCP

Horizon: mid

synesthesia exposes the platform's knowledge to agents: module documentation, `SN-` error
pages, roadmap status from `status.tel`, and migration instructions. An agent should never
need to clone the repository to answer "what does SN-042 mean?"

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
implementation. No tool in the ecosystem asks for YAML, JSON or HOCON configuration.

Done when: no ecosystem tool's own configuration surface accepts any format but TEL.
