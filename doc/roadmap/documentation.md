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
Baseline: 121 error types carry `SN-` codes, 39 do not; 233 error pages exist (measured 2026-08-01)

The `SN-` scheme covers the macro-raised compile errors; the remaining runtime error types get
codes and pages under the same never-reuse discipline, and parity between types, codes and
pages becomes a CI check so it cannot drift.

Done when: the parity check runs in CI and

    git grep -hE 'extends +Error\((m"|[a-z])' -- lib | wc -l    # 0

## doc-2: the messages standard is written

Horizon: near

`doc/standards/messages.md` is a self-declared stub, despite the message convention being
load-bearing across every module. The standard gets written: register, structure, vocabulary
and examples for `m"…"` messages and error text.

Done when:

    grep -c 'Stub' doc/standards/messages.md    # 0

## doc-3: the legacy documentation systems retire

Horizon: near
Baseline: 92 files still say "built by Fury"; 35 of 133 modules have no `lib/*/doc/` directory (measured 2026-08-01)

The per-module readme boilerplate predates the monorepo and describes a build tool that does
not yet exist again. Whatever in `lib/*/doc/` is worth keeping migrates into `doc/modules/`
topics; the rest is deleted rather than left to mislead.

Done when:

    git grep -l 'built by Fury' | wc -l    # 0

and no `lib/*/doc/` directory remains.

## doc-4: every module is covered by a topic

Horizon: mid
Baseline: 91 topics in `doc/modules/`; 35 modules have no documentation anywhere (measured 2026-08-01)

Every published module is covered by at least one `doc/modules/` topic, and a coverage script
maps modules to topics so orphans are visible.

Done when: the coverage script runs in the ordinary build and reports zero uncovered modules.

## doc-5: API documentation is generated and published

Horizon: mid
Baseline: every published `docJar` is empty (measured 2026-08-01)

The extraction pipeline is the shared foundation: fluence searches it (`tool-7`) and
API-derived versioning diffs it (`dist-4`). Published API documentation is the immediate
deliverable.

Done when: every published component's documentation artifact is non-empty, and the API
documentation is published for every release.

## doc-6: the website derives from `doc/`

Horizon: mid

The prose under `web/res/content/` duplicates philosophy material by hand. The website renders
from `doc/` — one source, no parallel copies.

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
