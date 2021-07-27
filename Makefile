
bin:
	mkdir -p bin

bin/cs: bin
	curl -fLo bin/cs https://git.io/coursier-cli-linux
	chmod +x bin/cs

bin/sbt: bin/cs
	bin/cs install sbt
	cp $${HOME}/.local/share/coursier/bin/sbt bin/sbt

bin/scalac: bin/sbt scala
	cd scala && bin/sbt dist/packArchive
	cp scala/bin/scalac bin/scalac

out:
	mkdir -p out

out/rudiments: mod/rudiments bin/scalac
	bin/scalac -d out rudiments/src/core/*.scala
