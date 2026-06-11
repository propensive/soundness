# Where `make flame` installs the `flame` REPL binary. Override with `make flame PREFIX=…`
# (the binary lands in `$(PREFIX)/bin`, which should be on your `PATH`).
PREFIX ?= $(HOME)/.local

publishLocal:
	./mill __.publishLocal

flame:
	./mill flame.client.bin
	mkdir -p $(PREFIX)/bin
	install -m 755 bin/flame $(PREFIX)/bin/flame
	@echo "flame installed to $(PREFIX)/bin/flame"

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

build:
	./mill groupCheck.validate
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

xeq-release:
	@if [ -z "$(VERSION)" ]; then echo "Usage: make xeq-release VERSION=X.Y.Z [REPO=owner/repo] [TAG=tag]" >&2; exit 1; fi
	./etc/ci/xeq-release.sh "$(VERSION)" "$(REPO)" "$(TAG)"

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

.PHONY: publishLocal flame build dev ci test matrix attest verify-attest push release xeq-release
