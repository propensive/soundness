FURY_VERSION=0.7.14
LIB=euphemism

compile: fury/bin/fury
	fury/bin/fury config set --theme full
	fury/bin/fury layer extract -f ${LIB}.fury
	fury/bin/fury permission grant -P b7a
	fury/bin/fury build run --https --output linear

install.sh:
	curl http://downloads.furore.dev/fury-${FURY_VERSION}.sh > install.sh

fury/bin/fury: install.sh
	chmod +x install.sh
	./install.sh fury

.PHONY: compile install
