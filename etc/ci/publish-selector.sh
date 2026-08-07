#!/usr/bin/env bash
#
# Prints the mill selector naming everything Soundness publishes: the bundles (with the `js` and
# `native` cross of each bundle that has platform-capable contents), the three standalone compiler
# plugins, and the universal `soundness` artifact.
#
# The ~230 components are deliberately NOT published: Maven Central limits the file count, size and
# frequency of deployments, and the components' classfiles now ship inside the bundle jars instead.
# So no `__.publishArtifacts`-style wildcard may be used anywhere in the release path;
# `publishableModules` in build.mill is the single source of truth, and this script is the only
# place that turns it into a selector.
#
# Usage: ./etc/ci/publish-selector.sh [task]   (task defaults to `publishArtifacts`)

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

task="${1:-publishArtifacts}"

modules=$(./mill show publishableModules 2>/dev/null \
  | python3 -c 'import json,sys; [print(x) for x in json.load(sys.stdin)]')

if [[ -z "$modules" ]]; then
  echo "publish-selector: computed an empty publish set; aborting" >&2
  exit 1
fi

printf '{%s}.%s\n' "$(printf '%s\n' "$modules" | paste -sd, -)" "$task"
