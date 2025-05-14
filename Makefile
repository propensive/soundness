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
	java -cp out/turbulence/test/assembly.dest/out.jar turbulence.Tests

.PHONY: publishLocal build dev ci test
