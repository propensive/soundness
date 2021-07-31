
pub: scala/bin/scalac bin/cs $(wildcard mod/*/)
	echo $<
	mkdir -p pub
	PATH="$(PATH):bin" etc/build.sh

bin/cs: bin
	curl -fLo bin/cs https://git.io/coursier-cli-linux
	chmod +x bin/cs

bin:
	mkdir -p bin

clean:
	rm -r pub out

scala/bin/scalac:
	git submodule update --init scala

mod/%/:
	git submodule update --init mod/$*

headers:
	for MOD in mod/*; do \
	  cat tmpl/header | envsubst > $$MOD/.header ; \
	  for FILE in $(shell find $$MOD/src -name '*.scala') ; do \
	    cat $$MOD/.header > $(TMP) ; \
	    sed '/\(package\|object\|import\)/,$$!d' "$$FILE" >> "$(TMP)" ; \
	    mv "$(TMP)" "$$FILE" ; \
	  done && rm $$MOD/.header ; \
	done

.PHONY: headers
