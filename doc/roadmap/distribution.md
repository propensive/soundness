# Distribution and Release

Trust in a platform is trust in its releases: that they arrive predictably, that they say what
changed, that breaking changes come with a way through, and that what you download is what was
built. Soundness already has the rarest of these — every release is attested by a signed,
verifiable note recording exactly what was built and tested — but the rest of the story is
thinner: release notes live only in pull-request bodies, and there is no published convention
for migrating across breaking changes, even though cheap migration is the platform's entire
answer to API stability.

Migration-first stability is the keystone. APIs change whenever a better or safer design
exists; what makes that tenable is that every breaking change ships with instructions precise
enough for an agent to execute against a downstream codebase. Beyond it lies distribution
itself: LIRA — one file per library release carrying every compiled representation, with a TEL
manifest, API-derived versioning and verifiable signatures — replaces Maven Central. Its
implementation began here as the reliquary library, but LIRA is a format and a tool in its own
right, like fury; it belongs in the `lira` repository, and this roadmap tracks only what
Soundness must do to be published through it (`dist-10`). Maven Central publishing has already been switched off: each tagged
version is published as a GitHub release whose assets are the individual component jars, an
interim channel until LIRA is live. Each released jar embeds its POM and ivy.xml, and
`make sync-releases` installs a release into `~/.ivy2/local`, from which Mill resolves the
components by version with no repository configuration. The gate requires attested LIRA
publishing to be live.

## dist-1: releases are changelogged

Horizon: near
Baseline: no changelog exists; release notes live in pull-request bodies (measured 2026-08-01)

The release notes already written per pull request accumulate into a changelog, and the
release refuses to run without one. A release is now cut by pushing a signed tag (#2040):
`.github/workflows/release.yml` hands over to the shared `scala-release.yml` in
propensive/.github, configured by `etc/release`.

Done when: a changelog file exists, and the release fails when the version being released
has no entry. Partly done: `release_notes.py` (propensive/.github) assembles every release's
**Changes** section from the body of each pull request merged since the previous tag, so the
changelog is built rather than kept; nothing yet *refuses* a release for an empty one.

## dist-2: the migration convention

Horizon: near

The keystone item: `doc/migration.md` defines the agent-executable
migration-instruction format — what changed, how to detect affected code, the exact rewrite,
and how to verify it — and CI enforces that breaking-labelled pull requests carry instructions
in that format. Several tracks terminate here: it is what `api-6` flows through and what
`tool-5` serves to agents.

Done when: `doc/migration.md` defines the convention, the release refuses to run without
`doc/migration/<version>.md`, and a CI check fails any pull request that changes `lib/**/src`
without touching `doc/migration/pending.md`. The first two hold: the convention is written,
`0.65.0.md` through `0.68.0.md` exist, and the `migration` directive in `etc/release` refuses
a tag while `pending.md` survives or the version's notes are missing. The pull-request check
is what remains: `main.yml` verifies the attestation and nothing else. The convention is not
label-based: every material change records itself in `pending.md`, and the release renames
the file.

## dist-3: LIRA exists

Horizon: near → mid
Baseline: reliquary reads and assembles `.lira` files, with unit round-trips of trees, atoms, deltas and manifests; no whole-release round-trip exists (measured 2026-09-25)

The specification gets its reference implementation: a reader and writer for `.lira` files
carrying classfiles, TASTy, Scala.js IR and Native IR with a TEL manifest, round-tripping the
current Soundness artifacts. reliquary (`Lira`, `LiraAssembler`) is that implementation, and
anthology, degustation, mandible and xenophile each carry a `lira` component; what the
criterion still wants is the scripted test over a real release, run against the
implementation wherever it lives after `dist-10`.

Done when: the reference implementation round-trips a Soundness release — every published
component packed into `.lira` files and unpacked byte-identically — in a scripted test.

## dist-4: API-derived versioning

Horizon: mid
Needs: doc-5

Version numbers become statements about compatibility, computed from the API surface via the
extraction pipeline, rather than assertions of intent.

Done when: the release pipeline computes each component's version from its extracted API
surface, and a scripted test demonstrates that an incompatible change forces the version it
should.

## dist-5: fury publishes LIRA

Horizon: mid
Needs: tool-4

Publishing is a build-tool concern: fury packs, signs and publishes `.lira` files as part of
its ordinary release flow.

Done when: `fury publish` produces and publishes signed `.lira` files for a real release.

## dist-6: the trust infrastructure

Horizon: mid → long

The rest of the LIRA distribution design: quantum-safe signatures, DNS-verified namespaces,
and a transparency log, at minimum-viable scale.

Done when: a `.lira` file's signature, namespace and transparency-log inclusion are all
verified by a single command shipped with the tooling.

## dist-7: LIRA publishing, attested

Horizon: mid
Needs: dist-3

Every release is published as `.lira` files, attested under the same git-note discipline as
the build itself. This is the gate's requirement: the channel that replaces Maven Central,
live and verifiable.

Done when: a release ships to LIRA, attested, and `make verify-attest` covers the published
artifacts.

## dist-8: Maven Central retired

Horizon: long
Needs: dist-5, dist-6, dist-7

Beyond the gate: fury resolves dependencies from LIRA, a fresh project builds with zero Maven
resolution, and the release pipeline no longer publishes to Maven Central at all.

Done when: the release script contains no Maven Central publishing step, and a fresh fury
project builds and runs with the Maven resolver disabled.

## dist-9: trust is verifiable by outsiders

Horizon: long

The release process is documented end-to-end so that a third party — not a maintainer — can
verify any release's attestation from public information alone.

Done when: the verification instructions are published, and a scripted third-party
verification (no maintainer credentials, public data only) passes for the latest release.

## dist-10: LIRA's implementation leaves this repository

Horizon: near
Baseline: `lib/reliquary` (7,387 lines in `core`, `derive` and `test`) plus the four `lira` adapter components — `anthology.lira`, `degustation.lira`, `mandible.lira`, `xenophile.lira` — and their tests (measured 2026-09-25)

reliquary was written here (#1700) because the libraries it needed were here, but nothing in
Soundness depends on it: its only in-tree consumers are the four adapter components that plug
each language's atomizer and discipline into its SPI, the `soundness.tool` bundle that lists
them, and the degustation and mandible suites that exercise those adapters. Developing LIRA is
not Soundness work, any more than developing fury is. reliquary and the four adapters move to
the `lira` repository, which pins Soundness as an ordinary consumer; the `library-archives`
topic, the umbrella export, the compatibility row and the doc-coverage entry go with them.

Done when:

    test -d lib/reliquary || echo absent                          # absent
    ls -d lib/*/src/lira 2>/dev/null | wc -l                      # 0
    git grep -c reliquary -- build.mill etc doc/modules | wc -l   # 0
