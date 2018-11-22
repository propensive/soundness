reversion:
	@for FILE in $(shell find src -name '*.scala'); do \
	  echo "Updating header in $$FILE" ; \
		sed "s/%VERSION%/$(shell cat .version)/g" ".header" | \
		    sed "s/%YEAR%/$(shell date +%Y)/g" > ".tmp.scala" ; \
		sed '/package/,$$!d' "$$FILE" >> ".tmp.scala" ; \
		mv ".tmp.scala" "$$FILE" ; \
	done
