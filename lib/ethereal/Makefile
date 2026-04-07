RUST_TARGETS := \
  x86_64-pc-windows-gnu \
  aarch64-pc-windows-gnullvm \
  x86_64-unknown-linux-gnu \
  aarch64-unknown-linux-gnu \
  x86_64-apple-darwin \
  aarch64-apple-darwin

LABELS := \
  windows-x64 \
  windows-arm64 \
  linux-x64 \
  linux-arm64 \
  macos-x64 \
  macos-arm64

BINARIES := \
  runner.exe \
  runner.exe \
  runner \
  runner \
  runner \
  runner

OUTPUT := bin/installer
DATA := res/data.jar
TMPL := src/installer/polyglot.tmpl

BAT := src/installer/polyglot.bat
PS1 := src/installer/polyglot.ps1
SH  := src/installer/polyglot.sh

target_bin = target/$(word $(1),$(RUST_TARGETS))/release/$(word $(1),$(BINARIES))

ALL_BINS := $(foreach i,1 2 3 4 5 6,$(call target_bin,$(i)))

STAMP := target/.zigbuild.stamp

.PHONY: all clean build

all: $(OUTPUT)

build: $(STAMP)

$(STAMP): src/runner/src/main.rs Cargo.toml
	cargo zigbuild --release $(foreach t,$(RUST_TARGETS),--target $(t))
	@touch $@

$(ALL_BINS): $(STAMP)

$(OUTPUT): $(TMPL) $(BAT) $(PS1) $(SH) $(ALL_BINS) $(DATA)
	@mkdir -p bin
	@echo "Assembling polyglot installer..."
	@awk '{ \
	    if ($$0 == "@@BAT@@") { while ((getline line < "$(BAT)") > 0) print line; close("$(BAT)") } \
	    else if ($$0 == "@@PS1@@") { while ((getline line < "$(PS1)") > 0) print line; close("$(PS1)") } \
	    else if ($$0 == "@@SH@@") { while ((getline line < "$(SH)") > 0) print line; close("$(SH)") } \
	    else print \
	  }' $(TMPL) > $@
	@set -e; \
	tmpdir=$$(mktemp -d); \
	trap 'rm -rf "$$tmpdir"' EXIT; \
	bins="$(call target_bin,1) $(call target_bin,2) $(call target_bin,3) $(call target_bin,4) $(call target_bin,5) $(call target_bin,6) $(DATA)"; \
	labels="windows-x64 windows-arm64 linux-x64 linux-arm64 macos-x64 macos-arm64 data"; \
	i=0; \
	for bin in $$bins; do \
	  i=$$((i + 1)); \
	  base64 < "$$bin" | fold -w 8000 > "$$tmpdir/$$i.b64"; \
	done; \
	offset=1; index="index:"; sep=""; \
	i=0; \
	for label in $$labels; do \
	  i=$$((i + 1)); \
	  n=$$(wc -l < "$$tmpdir/$$i.b64" | tr -d ' '); \
	  index="$${index}$${sep}$${label}=$${offset}"; \
	  sep=","; \
	  echo "  $$label	($$n lines, offset $$offset)"; \
	  offset=$$((offset + n + 2)); \
	done; \
	echo "$$index" >> $@; \
	for i in 1 2 3 4 5 6 7; do \
	  echo "-----BEGIN CERTIFICATE-----" >> $@; \
	  cat "$$tmpdir/$$i.b64" >> $@; \
	  echo "-----END CERTIFICATE-----" >> $@; \
	done; \
	echo '#>' >> $@
	@chmod +x $@
	@echo "Written to $@"

clean:
	cargo clean
	rm -f $(OUTPUT)
