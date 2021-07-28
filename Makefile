
bin:
	mkdir -p bin

bin/cs: bin
	curl -fLo bin/cs https://git.io/coursier-cli-linux
	chmod +x bin/cs

bin/sbt: bin/cs
	bin/cs install sbt
	cp /home/runner/.local/share/coursier/bin/sbt bin/sbt
	chmod +x bin/sbt

scala/bin/scalac: bin/sbt scala/src
	cd scala && ../bin/sbt dist/packArchive

out:
	mkdir -p out

scala/src:
	git submodule update --init scala

mod/rudiments/src:
	git submodule update --init mod/rudiments

mod/kaleidoscope/src:
	git submodule update --init mod/kaleidoscope

out/rudiments/core: out mod/rudiments/src scala/bin/scalac
	scala/bin/scalac \
	  -language:experimental.saferExceptions \
	  -new-syntax \
	  -Ysafe-init \
	  -feature \
	  -Xcheck-macros \
	  -Ycheck-all-patmat \
	  -Yexplicit-nulls \
	  -d out mod/rudiments/src/core/*.scala

out/kaleidoscope/core: out mod/kaleidoscope/src scala/bin/scalac pub/rudiments-core.jar
	scala/bin/scalac \
	  -classpath:pub/rudiments-core.jar \
	  -language:experimental.saferExceptions \
	  -new-syntax \
	  -Ysafe-init \
	  -feature \
	  -Xcheck-macros \
	  -Ycheck-all-patmat \
	  -Yexplicit-nulls \
	  -d out mod/kaleidoscope/src/core/*.scala
pub:
	mkdir -p pub

pub/rudiments-core.jar: pub out/rudiments/core
	jar cf pub/rudiments-core.jar -C out rudiments

pub/kaleidoscope-core.jar: pub out/kaleidoscope/core pub/rudiments-core.jar
	jar cf pub/kaleidoscope-core.jar -C out kaleidoscope

all: pub/rudiments-core.jar pub/kaleidoscope-core.jar

.PHONY: all
