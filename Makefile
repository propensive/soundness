SOURCES := $(wildcard src/core/**/*.java)

burdock.jar: compile etc/manifest res/META-INF
	jar cvmf etc/manifest burdock.jar -C .wrath/bin burdock -C res META-INF

.wrath/bin:
	mkdir -p .wrath/bin

compile: .wrath/bin $(SOURCES)
	javac -d .wrath/bin -Xlint:deprecation src/core/burdock/*.java

.PHONY: compile
