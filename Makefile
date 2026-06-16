publishLocal:
	./mill __.publishLocal

test:
	./mill test.assembly
	java -Xss2m -Xmx4g -cp out/test/assembly.dest/out.jar soundness.Tests

test.%:
	./mill clean $*.test
	./mill $*.test.assembly
	java -Xss2m -Xmx4g -cp out/$*/test/assembly.dest/out.jar $*.Tests

failing:
	./mill test.assembly
	java -Xss2m -Xmx4g -cp out/test/assembly.dest/out.jar soundness.FailingTests

check-givens:
	python3 etc/check-given-uniqueness.py

build:
	./mill groupCheck.validate
	python3 etc/check-given-uniqueness.py
	./mill soundness.all

dev:
	./mill -w soundness.all

ci:
	./etc/ci/run-tests.sh

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

xeq-build:
	@if [ -z "$(RUNNERS_VERSION)" ]; then echo "Usage: make xeq-build RUNNERS_VERSION=X" >&2; exit 1; fi
	./etc/ci/xeq-build.sh "$(RUNNERS_VERSION)"

runners-build:
	./etc/ci/runners-build.sh

runners-fetch:
	@if [ -z "$(RUNNERS_VERSION)" ]; then echo "Usage: make runners-fetch RUNNERS_VERSION=X [REPO=owner/repo]" >&2; exit 1; fi
	./etc/ci/runners-fetch.sh "$(RUNNERS_VERSION)" "$(REPO)"

runners-release:
	@if [ -z "$(RUNNERS_VERSION)" ]; then echo "Usage: make runners-release RUNNERS_VERSION=X [REPO=owner/repo]" >&2; exit 1; fi
	./etc/ci/runners-release.sh "$(RUNNERS_VERSION)" "$(REPO)"

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

.PHONY: publishLocal build dev ci test matrix attest verify-attest push release xeq-build runners-build runners-fetch runners-release
