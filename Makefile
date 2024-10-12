SOURCES := $(wildcard src/core/**/*.java)

burdock.jar: compile etc/manifest
	jar cvmf etc/manifest burdock.jar -C .wrath/bin burdock

.wrath/bin:
	mkdir -p .wrath/bin

compile: .wrath/bin $(SOURCES)
	javac -d .wrath/bin -Xlint:deprecation src/core/burdock/*.java

.PHONY: compile
