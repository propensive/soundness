publishLocal:
	./mill "$$(./mill show release.selector | tr -d '"').publishLocal"

# Tests and benchmarks are run by `fume` (https://github.com/propensive/fume), which discovers
# every suite in a built assembly from its META-INF/services/probably.Suite index. The classpath
# for `make test`/`make ci` comes from `.pyrocosm/fume/config.tel`; `$(TESTS)` are fume selection terms
# (test ids, name globs, axis constraints such as `N<=64`). Every run goes through
# `etc/ci/fume-run.sh`, which runs each suite in a JVM of its own (`--fork`) through the user's
# own fume daemon; fume gives each suite a temporary directory and deletes it when the suite's
# JVM exits, so a suite's forgotten scratch files never outlive the run.
#
# The compilers run in-process in mill's single JVM (`.mill-jvm-opts`: 8 GB), and the largest
# codec test modules (xylophone, stratiform, jacinta, ypsiloid, apoplexy: multi-megabyte
# classes, because every test re-expands an inline derivation) all land at the end of the
# graph together. Left to mill's default parallelism they exhaust the heap on a 16 GB machine,
# so the assemblies are built with the same job cap the attest build uses (`SOUNDNESS_CI_JOBS`
# in `etc/ci/attest.sh`); override it with `make test JOBS=8`.
JOBS ?= $(or $(SOUNDNESS_CI_JOBS),4)

# The Orthant command which reads the documentation store.
ORTHANT ?= orthant

test:
	./mill -j $(JOBS) test.assembly
	./etc/ci/fume-run.sh -c out/test/assembly.dest/out.jar $(TESTS)

test.%:
	./mill clean $*.test
	./mill -j $(JOBS) $*.test.assembly
	./etc/ci/fume-run.sh -c out/$*/test/assembly.dest/out.jar $(TESTS)

bench:
	./mill -j $(JOBS) bench.assembly
	./etc/ci/fume-run.sh --bench -c out/bench/assembly.dest/out.jar $(TESTS)

bench.%:
	./mill -j $(JOBS) $*.bench.assembly
	./etc/ci/fume-run.sh --bench -c out/$*/bench/assembly.dest/out.jar $(TESTS)

keywords:
	./mill keywords.assembly
	java -Xss2m -Xmx4g -cp out/keywords/assembly.dest/out.jar keywords.Analysis $(KEYWORDS_ARGS)

check-givens:
	python3 etc/check-given-uniqueness.py

check-while:
	python3 etc/check-while-count.py

check-escapes:
	python3 etc/check-escape-count.py

# The stdlib's `IArray` is not used: the frozen `Array[element]^{}` is the immutable array.
check-iarray:
	! grep -rnw --include='*.scala' --include='build.mill' IArray lib src tests build.mill

# Temporary files and directories are galilei's `Scratch` loans, which delete what they made when
# their scope ends, however it ends: never the JDK's `createTempFile`/`createTempDirectory`, whose
# leftovers a long-lived JVM (a test run inside fume's daemon) keeps until it exits, and never
# `deleteOnExit`, which waits for that exit too. Four files cannot use a loan, and delete what
# they make in a `finally` instead: prescience's staging compiler and abscission's shrinking
# engine, which galilei is above; sedentary's profiler, whose quoted body runs in a JVM of its
# own, compiled where galilei's inline code does not resolve; and beneficence's compiler plugin,
# whose file beside its target is moved into place rather than thrown away.
check-scratch:
	! grep -rnE --include='*.scala' 'createTemp(File|Directory)|deleteOnExit' lib src tests \
	  | grep -v '^lib/prescience/src/core/prescience.internal.scala:' \
	  | grep -v '^lib/abscission/src/scala-3.10/abscission.Shrinker.scala:' \
	  | grep -v '^lib/sedentary/src/core/sedentary.Profile.scala:' \
	  | grep -v '^lib/beneficence/src/plugin/beneficence.GivensWriter.scala:'

# The unsafety census: the counts of every rule in .pyrocosm/flare/config.tel, over the sources,
# as `flare metrics` records them in git notes commit by commit. Reports only: nothing here fails.
unsafety:
	flare metrics --dry-run

# The documentation is held in the Orthant store in .orthant (.orthant/readme.md). The English
# which is not yet released is written in doc, the store's live tree, and `orthant tag` stores
# it. The scripts which read the documentation read the English at `main` as the store has it,
# exported to out/doc, so that they and the translations are checked against one text; an edit
# in doc which `orthant tag` has not stored is refused here, before anything reads a stale copy.
docs:
	rm -rf out/doc
	$(ORTHANT) export out/doc en main
	@diff -rq out/doc doc >/dev/null || { \
	  echo "docs: doc/ differs from the Orthant store; store it with \`orthant tag\` (\`orthant status\` lists the edits)" >&2; \
	  exit 1; }

# The coercion-helper census: small defs that only re-wrap a value, found from the parse tree and
# judged by a model against doc/standards/plumbing.md (read from out/doc), recorded in git notes under
# refs/notes/flare-assess/plumbing (rule `plumbing` in .pyrocosm/flare/config.tel). Needs the
# `anthropic` credential in Pyrocosm's cascade; nothing here fails.
plumbing: docs
	flare assess plumbing $(PLUMBING_ARGS)

build: docs
	./mill groupCheck.validate
	$(MAKE) check-iarray
	$(MAKE) check-scratch
	python3 etc/check-given-uniqueness.py
	python3 etc/check-doc-coverage.py
	python3 etc/check-compatibility.py
	python3 etc/check-while-count.py
	python3 etc/check-escape-count.py
	python3 etc/check-example-data.py
	./mill soundness.all
	./mill benches.jar

dev:
	./mill -w soundness.all

ci:
	./etc/ci/fume-run.sh -c out/test/assembly.dest/out.jar

wasm-e2e:
	./etc/ci/wasm-e2e.sh

# Evaluate every tutorial example in doc/soundness/modules/ through the flame REPL (`make doccheck`, or
# `make doccheck DOC=json` for one tutorial), and check the names the examples use against the
# source. See etc/doccheck.py for what it needs and how it reads the results.
doccheck: docs
	python3 etc/doccheck-names.py $(DOC)
	python3 etc/doccheck.py $(DOC)

# Compile every tutorial's fences with fuel, each document as one session, against the test
# assembly `.pyrocosm/fuel/config.tel` names (`./mill test.assembly` builds it). Fuel is the
# release pinned in etc/tools, which `make tools` installs, or FUEL names another. Exit status 4
# means a fence did not do as its preamble said.
FUEL ?= fuel
fuelcheck:
	./mill test.assembly + ambience.wasi.jar + aviation.wasi.jar + capricious.wasi.jar + coaxial.wasi.jar + galilei.wasi.jar + telekinesis.wasi.jar + turbulence.wasi.jar + telekinesis.forms.jar + breviloquence.http.jar + caesura.http.jar + locomotion.http.jar + eucalyptus.gcp.jar + caduceus.resend.jar + orthodoxy.server.jar
	$(FUEL) compile

attest:
	./etc/ci/attest.sh

verify-attest:
	./etc/ci/verify-attest.sh

push:
	git push
	git push origin refs/notes/ci-attestation

# Releases are cut by tagging, not by make. There is no version to bump — `publishVersion` comes
# from the tag — and nothing to merge first, since the notes accumulate in
# etc/migration/<version>.md from the start: `git tag -s X.Y.Z && git push --tags`. The tag
# fires .github/workflows/release.yml, which runs the shared release.sh in propensive/.github: it
# gates on a signed tag, on CI already being green on that commit (which verifies the attestation
# note), on the migration notes for that version, on groupCheck, and on every pin being a release;
# then drafts the release, uploads six hundred jars in batches, checks every digest, and
# publishes. If anything fails, the release and the tag are both deleted. See etc/release. This
# target survives only to say so.
release:
	@echo "Releases are triggered by tags, not by make. Merge the migration-notes rename, then:" >&2
	@echo "" >&2
	@echo "    git tag -s X.Y.Z && git push --tags" >&2
	@echo "" >&2
	@echo "See propensive/.github." >&2
	@exit 1

# Install the commands pinned in etc/tools (fume, flare, flame, fuel, orthant) through their releases' installers.
tools:
	./etc/shared tools.sh

# Publish HEAD's jars as a snapshot — a `snapshot-<hex>` pre-release named by the filtered tree
# of the commit, at version `<next minor>-<hex>` — for Pyrocosm (or anything else) to pin in its
# etc/refs before the next release. `LOCAL=1` stages and installs without publishing. The
# last line printed is the pin. See snapshot.sh in propensive/.github.
snapshot:
	./etc/shared snapshot.sh soundness "$$(git describe --tags --abbrev=0 --match '[0-9]*.[0-9]*.[0-9]*' | awk -F. '{print $$1"."$$2+1".0"}')"

# Delete snapshot pre-releases older than DAYS (default 60) days.
snapshot-prune:
	./etc/shared snapshot-prune.sh soundness $(DAYS)

# Fetch the pinned `xek` builder (etc/xek.tsv) from the propensive/xek release into dist/xek,
# verified against its SHA-256. The build shells out to it for packaging.
xek-fetch:
	./etc/ci/xek-fetch.sh

scala/%:
	TAG=$(word 1, $(subst :, ,$*)); \
	JDK=$(word 2, $(subst :, ,$*)); \
	docker build --build-arg JDK=$${JDK} --build-arg TAG=$${TAG} -t "scala:$${TAG}-$${JDK}" -f img/scala .

image/%: scala/%
	TAG=$(word 1, $(subst :, ,$*)); \
	JDK=$(word 2, $(subst :, ,$*)); \
	docker build --build-arg JDK=$${JDK} --build-arg TAG=$${TAG} -t "soundness:$${TAG}-$${JDK}" -f img/soundness .

boot:
	mkdir boot

bootstrap/%: boot image/%
	TAG=$(word 1, $(subst :, ,$*)); \
	JDK=$(word 2, $(subst :, ,$*)); \
	CID=$$(docker create soundness:$${TAG}-$${JDK}); \
	docker cp "$${CID}:/opt/soundness/soundness.jar" boot/soundness-$${TAG}.jar; \
	docker rm $${CID}}

matrix:
	@$(foreach jdk,23 24, \
	    $(foreach scala,3.6.1 3.6.2 3.6.3 3.6.4 3.7.0 3.7.1 3.7.1 main, \
			    $(MAKE) bootstrap/$(scala):$(jdk);))

.PHONY: publishLocal docs plumbing build dev ci check-givens check-while check-escapes check-scratch unsafety wasm-e2e doccheck fuelcheck test bench matrix attest verify-attest push release tools snapshot snapshot-prune xek-fetch
