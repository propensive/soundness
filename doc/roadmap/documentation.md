# Documentation and Learnability

Documentation is part of the safety story. An undocumented API invites guessing, and guessing
is how surprising failures happen — for people and, more acutely, for AI agents, which will
confidently synthesise whatever the documentation fails to say. The standard is total coverage:
every module documented, every error code explained, every novel idea specified, and every
message following a written convention.

The prose exists in three parallel systems today: the topic guides in `doc/modules/` — the
direction of travel, and where the substantial writing happens — the legacy per-module
templates in `lib/*/doc/`, inherited from when each module was its own repository, and a
separate copy of the philosophy material under `web/res/content/`. Three systems means
divergence, and divergence means one of them is lying. This track collapses them to one,
completes coverage, and adds the layer that prose cannot provide: generated, searchable API
documentation — infrastructure that fluence and LIRA's API-derived versioning also depend on.
It ends with the strongest learnability test there is: an agent, given only the documentation,
builds and deploys a working application.

## doc-1: every error type has a code and a page

Horizon: near
Baseline: 41 error types carry no `SN-` code; 247 error pages exist (measured 2026-10-21; 40 and 241 on 2026-09-25; 39 and 233 on 2026-08-01)

The `SN-` scheme covers the macro-raised compile errors; the remaining runtime error types get
codes and pages under the same never-reuse discipline, and parity between types, codes and
pages becomes a CI check so it cannot drift. Nothing in `etc/` checks the parity yet.

Done when: the parity check runs in CI and

    git grep -hE 'extends +Error\((m"|[a-z])' -- lib | wc -l    # 0

## doc-2: the messages standard is written

Done: #2306 (2026-10-21)

`doc/standards/messages.md` was a self-declared stub, despite the message convention being
load-bearing across every module. The standard is now written: register, structure, vocabulary,
what a message must not contain, composition through `Communicable`, and log events.

Done when:

    grep -c 'Stub' doc/standards/messages.md    # 0

## doc-3: the legacy documentation systems retire

Done: #1791 (2026-08-14)
Baseline was 92 files of per-module readme boilerplate, and 98 `lib/*/doc/` directories
(measured 2026-08-01). The boilerplate predated the monorepo; whatever in `lib/*/doc/` was
worth keeping moved into `doc/modules/` topics, and the rest was deleted rather than left to
mislead. No `lib/*/doc/` directory remains, and the boilerplate phrase matches nothing.

## doc-4: every module is covered by a topic

Done: #2306 (2026-10-21)
Baseline was 99 topics in `doc/modules/` and 1 of 144 libraries uncovered (measured 2026-09-25; 91 topics and 35 uncovered on 2026-08-01); 101 topics cover 145 of 145 on 2026-10-21

Every published module is covered by at least one `doc/modules/` topic, and a coverage script
maps modules to topics so orphans are visible: `etc/check-doc-coverage.py`, run by `make
build`, exits non-zero on an orphan. The last one, burdock, is covered by `packaging.md`; the
`COVERED_BY` entry that named that topic for ziggurat (a library #1973 removed) now names
burdock.

Done when:

    python3 etc/check-doc-coverage.py    # every library covered, exit 0

## doc-5: API documentation is generated and published

Horizon: mid
Baseline: every published `docJar` is empty — `build.mill` overrides the task to write an empty jar (measured 2026-09-25, unchanged since 2026-08-01)

The extraction pipeline is the shared foundation: fluence searches it (`tool-7`) and
API-derived versioning diffs it (`dist-4`). Published API documentation is the immediate
deliverable.

Done when: every published component's documentation artifact is non-empty, and the API
documentation is published for every release.

## doc-6: the website derives from `doc/`

Horizon: mid

The prose under `web/res/content/` duplicates philosophy material by hand: ten pages, last
touched in #801, each an older and shorter copy of a `doc/philosophy/` page. The website
renders from `doc/` — one source, no parallel copies.

Done when: `web/res/content/` contains no hand-maintained duplicate of any `doc/` page.

## doc-7: novel ideas are one hop away

Horizon: long

Every novel idea in Soundness — capture checking in practice, separation-checked mutability,
delimited scopes, declared errors — has a philosophy page, and every API that embodies the idea
links to it. A reader is never more than one hop from the explanation.

Done when: a link check verifies that each philosophy page is referenced from the topic guides
of the modules that embody it, and it runs in the ordinary build.

## doc-8: the walkthrough

Horizon: long
Needs: tool-2, tool-4

The integrative test of learnability, and the gate's final criterion: a from-nothing tutorial
that an agent can execute end-to-end — new project, build with fury, test with fume, deploy a
JVM-and-JavaScript web application — using only `doc/` and ecosystem tools.

Done when: the walkthrough is executed by an agent in CI and the run is recorded with each
release.
