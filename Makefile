publishLocal:
	./mill "$$(./mill show release.selector | tr -d '"').publishLocal"

# Tests and benchmarks are run by `fume` (https://github.com/propensive/fume), which discovers
# every suite in a built assembly from its META-INF/services/probably.Suite index. The classpath
# for `make test`/`make ci` comes from `.pyrocosm/fume/config.tel`; `$(TESTS)` are fume selection terms
# (test ids, name globs, axis constraints such as `N<=64`).
test:
	./mill test.assembly
	fume run -c out/test/assembly.dest/out.jar $(TESTS)

test.%:
	./mill clean $*.test
	./mill $*.test.assembly
	fume run -c out/$*/test/assembly.dest/out.jar $(TESTS)

bench:
	./mill bench.assembly
	fume run --bench -c out/bench/assembly.dest/out.jar $(TESTS)

bench.%:
	./mill $*.bench.assembly
	fume run --bench -c out/$*/bench/assembly.dest/out.jar $(TESTS)

keywords:
	./mill keywords.assembly
	java -Xss2m -Xmx4g -cp out/keywords/assembly.dest/out.jar keywords.Analysis $(KEYWORDS_ARGS)

check-givens:
	python3 etc/check-given-uniqueness.py

check-stdlib:
	./etc/check-stdlib-count.sh

check-while:
	python3 etc/check-while-count.py

# The unsafety census: the counts of every rule in .pyrocosm/flair/config.tel, over the sources,
# as `flair metrics` records them in git notes commit by commit. Reports only: nothing here fails.
unsafety:
	flair metrics --dry-run

build:
	./mill groupCheck.validate
	python3 etc/check-given-uniqueness.py
	python3 etc/check-doc-coverage.py
	./etc/check-stdlib-count.sh
	python3 etc/check-while-count.py
	./mill soundness.all
	./mill benches.compile

dev:
	./mill -w soundness.all

ci:
	fume run -c out/test/assembly.dest/out.jar

wasm-e2e:
	./etc/ci/wasm-e2e.sh

# Evaluate every tutorial example in doc/modules/ through the flame REPL (`make doccheck`, or
# `make doccheck DOC=json` for one tutorial), and check the names the examples use against the
# source. See etc/doccheck.py for what it needs and how it reads the results.
doccheck:
	python3 etc/doccheck-names.py $(DOC)
	python3 etc/doccheck.py $(DOC)

attest:
	./etc/ci/attest.sh

verify-attest:
	./etc/ci/verify-attest.sh

push:
	git push
	git push origin refs/notes/ci-attestation

release:
	@if [ -z "$(VERSION)" ]; then echo "Usage: make release VERSION=X.Y.Z" >&2; exit 1; fi
	./etc/ci/release.sh "$(VERSION)"

# Install every library pinned in etc/refs (none: Soundness depends on nothing propensive) and
# the jars of every tool pinned in etc/tools (the flair compiler plugin) into the local ivy
# repository, as CI does.
sync-deps:
	./etc/shared sync-deps.sh

# Install the commands pinned in etc/tools (fume, flair) through their releases' installers.
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

# Fetch the pinned `xeq` builder script (etc/xeq.tsv) from the propensive/xeq release into
# dist/xeq, verified against its SHA-256. The build shells out to it for packaging.
xeq-fetch:
	./etc/ci/xeq-fetch.sh

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

.PHONY: publishLocal build dev ci check-givens check-stdlib check-while unsafety wasm-e2e doccheck test bench matrix attest verify-attest push release sync-deps tools snapshot snapshot-prune xeq-fetch
