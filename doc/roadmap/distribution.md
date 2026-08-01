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
manifest, API-derived versioning and verifiable signatures — is specified but unimplemented,
and replaces Maven Central. The gate requires attested LIRA publishing to be live; switching
Maven Central publishing off lies just beyond it.

## dist-1: releases are changelogged

Horizon: near
Baseline: no changelog exists; release notes live in pull-request bodies (measured 2026-08-01)

The release notes already written per pull request accumulate into a changelog, and the
release script refuses to release without one.

Done when: a changelog file exists, and `etc/ci/release.sh` fails when the version being
released has no entry.

## dist-2: the migration convention

Horizon: near

The keystone item: `doc/standards/migration.md` defines the agent-executable
migration-instruction format — what changed, how to detect affected code, the exact rewrite,
and how to verify it — and CI enforces that breaking-labelled pull requests carry instructions
in that format. Several tracks terminate here: it is what `api-6` flows through and what
`tool-5` serves to agents.

Done when: `doc/standards/migration.md` exists without a stub marker, and a CI check rejects
breaking-labelled pull requests lacking conforming migration instructions.

## dist-3: LIRA exists

Horizon: near → mid

The specification gets its reference implementation: a reader and writer for `.lira` files
carrying classfiles, TASTy, Scala.js IR and Native IR with a TEL manifest, round-tripping the
current Soundness artifacts.

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
