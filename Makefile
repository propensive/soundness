
bin:
	mkdir -p bin

bin/cs: bin
	curl -fLo bin/cs https://git.io/coursier-cli-linux
	chmod +x bin/cs

bin/sbt: bin/cs
	bin/cs install sbt
	cp /home/runner/.local/share/coursier/bin/sbt bin/sbt
	chmod +x bin/sbt

bin/scalac: bin/sbt scala/src
	cd scala && bin/sbt dist/packArchive
	cp scala/bin/scalac bin/scalac

out:
	mkdir -p out

scala/src:
	git submodule update --init scala

mod/rudiments/src:
	git submodule update --init mod/rudiments

out/rudiments: out mod/rudiments/src bin/scalac
	bin/scalac -d out rudiments/src/core/*.scala
