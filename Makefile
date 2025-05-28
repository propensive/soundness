publishLocal:
	mill publishing.local

test:
	mill test.assembly
	java -cp out/test/assembly.dest/out.jar soundness.Tests

failing:
	mill test.assembly
	java -cp out/test/assembly.dest/out.jar soundness.FailingTests

build:
	mill soundness.all

dev:
	mill -w soundness.all

ci:
	java -cp out/test/assembly.dest/out.jar soundness.Tests

scala3:
	docker build -t scala3 -f img/scala3 .

image: scala3
	docker build -t soundness -f img/soundness .

.PHONY: publishLocal build dev ci test scala3 image
