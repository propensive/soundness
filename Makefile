
pub: scala/bin/scalac bin/cs $(wildcard mod/*/)
	echo $<
	mkdir -p pub
	PATH="$(PATH):bin" etc/build.sh

bin/cs: bin
	curl -fLo bin/cs https://git.io/coursier-cli-linux
	chmod +x bin/cs

bin:
	mkdir -p bin

scala/bin/scalac:
	git submodule update --init scala

mod/%/:
	git submodule update --init mod/$*
