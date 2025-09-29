publishLocal:
	mill publishing.local

test:
	mill test.assembly
	java -cp out/test/assembly.dest/out.jar soundness.Tests

test.exoskeleton:
	mill exoskeleton.test.assembly
	java -cp out/exoskeleton/test/assembly.dest/out.jar exoskeleton.Tests

failing:
	mill test.assembly
	java -cp out/test/assembly.dest/out.jar soundness.FailingTests

build:
	mill soundness.all

dev:
	mill -w soundness.all

ci:
	java -cp out/test/assembly.dest/out.jar soundness.Tests

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

.PHONY: publishLocal build dev ci test matrix
