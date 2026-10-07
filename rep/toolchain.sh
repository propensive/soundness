#!/usr/bin/env bash
# The pinned proscala compiler, runnable from a shell:  rep/toolchain.sh <dotc arguments...>
#
# Resolves the toolchain `build.mill` compiles with — the release tag in `settings.scalaRelease`
# (overridable with SOUNDNESS_SCALA_RELEASE, exactly as for Mill; SOUNDNESS_SCALA_HOME names a
# locally-built `release` directory instead), cached by the build under
# ~/.cache/soundness/proscala/<tag>/lib — and runs `dotc` from it with the fork's opt-in repairs
# (the `-Z` flags of `settings.scalaOptions`, read from build.mill so the two cannot drift), so
# that a reproduction sees the same checker as the build does. A fix that is opt-in shows RED
# without its flag (#1932 hit this in larceny), which is why the flags are never left to the
# caller. The release's `lib/` holds only the fork's own jars; the compiler's third-party runtime
# dependencies (ASM for the backend, the sbt interfaces for the bridge) are resolved with
# coursier, per stream, as `trait Toolchain` in build.mill does.
#
#   rep/toolchain.sh -version
#   rep/toolchain.sh -experimental -Ycc-new -d /tmp/out Foo.scala
#   rep/toolchain.sh --classpath                                  # print the compiler classpath
#   rep/toolchain.sh --flags                                      # print the -Z flags
#   REP_STOCK=1 rep/toolchain.sh ...                              # omit the -Z repairs
#   SOUNDNESS_SCALA_RELEASE=3.10.1-dev-p17 rep/toolchain.sh ...   # the other stream
#
# The compiler's own jars are also put on the compilation classpath (`-usejavacp`), so a
# `-classpath` argument only needs to name the reproduction's own prerequisites.
set -euo pipefail
root=$(cd "$(dirname "$0")/.." && pwd)
build="$root/build.mill"

release=${SOUNDNESS_SCALA_RELEASE:-$(sed -n 's/.*"SOUNDNESS_SCALA_RELEASE", "\([^"]*\)".*/\1/p' "$build" | head -1)}
if [[ -n "${SOUNDNESS_SCALA_HOME:-}" ]]; then lib="$SOUNDNESS_SCALA_HOME/lib"
else lib="$HOME/.cache/soundness/proscala/$release/lib"; fi

if [[ ! -f "$lib/scala3-compiler.jar" ]]; then
  echo "rep/toolchain.sh: no compiler at $lib" >&2
  echo "  fetch it with:  SOUNDNESS_SCALA_RELEASE=$release ./mill toolchain.distribution" >&2
  exit 2
fi

if [[ "$release" == 3.10* ]]
then asm=(org.ow2.asm:asm-util:9.10.1 org.ow2.asm:asm-commons:9.10.1)
else asm=(org.scala-lang.modules:scala-asm:9.9.0-scala-1); fi
third=$(cs fetch --classpath "${asm[@]}" \
          org.scala-sbt:compiler-interface:1.12.0 org.scala-sbt:util-interface:1.11.5)

cp=""
for jar in scala3-compiler scala3-interfaces tasty-core scala3-library scala-library \
           proscala-library scala3-directives-parser scala3-staging; do
  [[ -f "$lib/$jar.jar" ]] && cp="$cp${cp:+:}$lib/$jar.jar"
done
cp="$cp:$third"

zflags=()
if [[ -z "${REP_STOCK:-}" ]]; then
  while IFS= read -r flag; do zflags+=("$flag"); done \
    < <(grep -oE '"-Z[a-z-]+"' "$build" | tr -d '"' | sort -u)
fi

case "${1:-}" in
  --classpath) echo "$cp"; exit 0 ;;
  --flags)     printf '%s\n' "${zflags[@]}"; exit 0 ;;
esac

# A repair already named by the caller (captured `opts.txt` carry the build's options) is not
# repeated.
extra=()
for flag in "${zflags[@]}"; do
  present=0
  for arg in "$@"; do [[ "$arg" == "$flag" ]] && present=1; done
  [[ $present == 0 ]] && extra+=("$flag")
done

exec java -cp "$cp" dotty.tools.dotc.Main -usejavacp "${extra[@]}" "$@"
