#!/usr/bin/env bash
# Regenerates yossarian's shell fixtures, in lib/yossarian/res/test/yossarian/shells: for each of
# bash, zsh, fish and PowerShell, `<shell>.transcript` holds what the shell wrote to its terminal
# while a short script was typed at it, and `<shell>.screen` the screen tmux shows after replaying
# that transcript, which yossarian's tests take as an independent judgement of how it renders.
#
# Needs the four shells and tmux on the path. Host names in OSC 7 working-directory reports are
# replaced with `localhost`, so that the fixtures do not depend on the machine they came from.
set -euo pipefail
cd "$(dirname "$0")/.."

out=lib/yossarian/res/test/yossarian/shells
mkdir -p "$out"

classpath=$(./mill --ticker false show guillotine.core.runClasspath | python3 -c '
import json, sys
paths = json.load(sys.stdin)
print(":".join(p if p.startswith("/") else p[p.index(":/") + 1:] for p in paths))')

java --enable-native-access=ALL-UNNAMED -cp "$classpath" etc/CaptureShellTranscripts.java "$out"
rm -f "$out"/*.log

for shell in bash zsh fish pwsh; do
  python3 - "$out/$shell.transcript" <<'PYTHON'
import re, sys
path = sys.argv[1]
data = open(path, 'rb').read()
open(path, 'wb').write(re.sub(rb'file://[^/\x07\x1b]+/', b'file://localhost/', data))
PYTHON

  # Echo is off in the replaying pane, so that tmux's own answers to the transcript's queries are
  # not echoed onto the screen it is capturing.
  session="yossarian-fixture-$shell"
  tmux new-session -d -s "$session" -x 80 -y 24 "stty -onlcr -echo; cat $out/$shell.transcript; sleep 30"
  sleep 1.5
  tmux capture-pane -p -t "$session" | sed 's/ *$//' | sed -e :a -e '/^\n*$/{$d;N;ba' -e '}' > "$out/$shell.screen"
  tmux kill-session -t "$session"
done
