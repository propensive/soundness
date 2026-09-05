#!/usr/bin/env python3
"""Evaluate every ```scala example in the tutorials under doc/modules/ through the flame REPL,
and report each fence as ran, rejected, or thrown.

flame (the Soundness REPL, https://github.com/propensive/flame) exposes its engine as a JSON
API when started with `flame serve`: `POST /api/sessions` creates a named session and
`POST /api/sessions/<name>/eval` with `{"code": "..."}` evaluates a whole submission in it,
answering `{"status": "ran"|"error", "value", "tpe", "output", "name", "diagnostics"}`. Each
tutorial runs in a fresh session, so its samples follow on from one another exactly as
doc/standards/style.md promises they do, and nothing leaks between tutorials.

flame is built against the latest Soundness *release*, while the tutorials document `main`, so
a rejection can be honest drift rather than a stale example: a diagnostic that names an
identifier recorded in doc/migration/pending.md is reported as TOLERATED; one whose only
complaints are `Not found` names — a placeholder the prose introduced but no fence defined —
as UNDEFINED; and everything else as STALE. A line carrying a `// does not compile` comment is
submitted on its own and must be rejected; one that compiles is reported as NEGATIVE-FAILED. A
runtime exception is THREW, and a fence that gives no reply within --timeout seconds is HUNG (the
session is then rebuilt and the fences before it replayed, so later fences still see them).

Because flame bundles only a curated subset of the Soundness components, every JVM component
jar of the release is added to the session with `/classload` first (from ~/.ivy2/local, where
`make sync-releases` installs a release). Entries added this way come last on the classpath,
so they only supplement what flame already has.

Usage:
    python3 etc/doccheck.py                     # every tutorial
    python3 etc/doccheck.py json time           # named tutorials
    python3 etc/doccheck.py --values quantities # also print each fence's value and type
Run it with `make doccheck` (or `make doccheck DOC=json`). Needs `flame` on the PATH (or the
FLAME environment variable) and the release synced into ~/.ivy2/local.
"""
import argparse, glob, json, os, re, socket, subprocess, sys, time, urllib.request, urllib.error

FLAME = os.environ.get('FLAME', 'flame')
IVY = os.path.expanduser('~/.ivy2/local/dev.propensive')
FENCE_OPEN = re.compile(r'^```scala\s*$')
FENCE_CLOSE = re.compile(r'^```\s*$')
NEGATIVE = re.compile(r"//\s*(?:does\s+not|doesn't|will\s+not|won't|cannot|can't)\s+compile\b", re.I)
IDENT = re.compile(r'`([^`]+)`')
WORD = re.compile(r'[A-Za-z_][\w]*')
ANSI = re.compile(r'\x1b\[[0-9;?]*[A-Za-z]')
PLATFORM_ONLY = {'wasi', 'plugin'}
CAMEL = re.compile(r'[a-z]+[A-Z]\w*|[A-Z]\w*\.[A-Z]\w*')

def release_version():
    """The release flame is built against. A full release syncs every component into
    ~/.ivy2/local, whereas a handful of bundle or plugin jars may sit there at a later version,
    so the most-populated version is the one to use."""
    counts = {}
    for path in glob.glob(os.path.join(IVY, '*', '*')):
        version = os.path.basename(path)
        if re.fullmatch(r'\d+\.\d+\.\d+', version):
            counts[version] = counts.get(version, 0) + 1
    if not counts:
        return None
    return max(counts, key=lambda v: (counts[v], tuple(int(n) for n in v.split('.'))))

def component_jars(version):
    """Every JVM component jar of `version` in ~/.ivy2/local (the Scala.js and Native variants
    carry a platform suffix and are skipped)."""
    jars = []
    for path in sorted(glob.glob(os.path.join(IVY, '*', version, 'jars', '*.jar'))):
        component = path.split(os.sep)[-4]
        if component.endswith('_sjs1_3') or component.endswith('_native0.5_3'):
            continue
        # A WASI backend cannot load into a JVM session (its package object links against WASI
        # imports the JVM lacks), and the bundles and compiler plugins add nothing.
        if component.rsplit('-', 1)[-1] in PLATFORM_ONLY or component.startswith('soundness'):
            continue
        jars.append(path)
    return jars

RENAMED = re.compile(r'`(?:[a-z]\w*\.)?([a-z]\w*)\.([A-Za-z]\w*)`(?:\s*\([^)]*\))?\s+renamed\s+to\s+`([A-Za-z]\w*)`')
RENAMED_GROUP = re.compile(r'`(?:[a-z]\w*\.)?([a-z]\w*)\.\{([^}]*)\}`(?:\s*\([^)]*\))?\s+renamed\s+to\s+`\{([^}]*)\}`')
FLATTENED = re.compile(r'`([a-z]\w*)\.([a-z]\w*)\.([A-Za-z]\w*)` → `([A-Za-z]\w*)`')

def release_names():
    """(family, new name) → old name, for every given the migration notes say was renamed since
    the last release; the driver retries a failed import under the old name."""
    older = {}
    try:
        text = open('doc/migration/pending.md', encoding='utf-8').read().replace('\n', ' ')
    except OSError:
        return older
    for family, old, new in RENAMED.findall(text):
        older[(family, new)] = old
    for family, olds, news in RENAMED_GROUP.findall(text):
        for old, new in zip([n.strip() for n in olds.split(',')], [n.strip() for n in news.split(',')]):
            older[(family, new)] = old
    for family, subfamily, old, new in FLATTENED.findall(text):
        older[(family, new)] = f'{subfamily}.{old}'
    # A whole choice package renamed, or a nested family flattened to one level: every member of
    # the new family maps to the same member of the old.
    for old, new in re.findall(r'[Cc]hoice package `(?:[a-z]\w*\.)*([a-z]\w*)` renamed to `(?:[a-z]\w*\.)*([a-z]\w*)`', text):
        older[(new, '*')] = old
    # A member flattened from a nested object (`dereferenceSymlinks.enabled` → `dereferenceSymlinks`)
    # keeps its family; an object member replaced by a choice package moves families.
    for old, new in re.findall(r'`([a-z]\w*\.[a-z]\w*)`\s+→\s+`([a-z]\w*)`', text):
        older[('*', new)] = old
        # The same shape also records a nested family flattened to one (`interfaces.paths` →
        # `pathInterfaces`): as a family, its members keep their names under the old path.
        older.setdefault((new, '*'), old)
    for old, new in re.findall(r'`(?:[a-z]\w*\.)*([a-z]\w*\.[a-z]\w*)` \(object member\) replaced by `(?:[a-z]\w*\.)*([a-z]\w*\.[a-z]\w*)`', text):
        family, member = new.split('.')
        older[(family, member)] = '=' + old
    for old, new in re.findall(r'`(?:[a-z]\w*\.)*([a-z]\w*\.[a-z]\w*)` replaced by `(?:[a-z]\w*\.)*([a-z]\w*\.[a-z]\w*)`', text):
        family, member = new.split('.')
        older.setdefault((family, member), '=' + old)
    # A whole family renamed with a dotted old path (`honeycomb.doms.html` (…) renamed to
    # `honeycomb.htmlDoms`): members and sub-packages keep their names under the old path.
    for old, new in re.findall(r'`(?:[a-z]\w*\.)?([a-z]\w*\.[a-z]\w*)`(?:\s*\([^)]*\))?\s+renamed\s+to\s+`(?:[a-z]\w*\.)?([a-z]\w*)`', text):
        older.setdefault((new, '*'), old)
    for family, suffix in re.findall(r'Every member of `(?:[a-z]\w*\.)*([a-z]\w*)` gains the suffix `(\w+)`', text):
        for old, new in re.findall(r'`([a-z]\w*)` → `([a-z]\w*' + suffix + r')`', text):
            older[(family, new)] = old
        for old in re.findall(r'`([a-z]\w*)`(?=,| become)', text):
            older.setdefault((family, old + suffix), old)
    return older

def migration_names():
    """Every identifier mentioned in doc/migration/pending.md: a diagnostic naming one of them
    is drift between the release and `main`, not a stale example."""
    names = set()
    try:
        text = open('doc/migration/pending.md', encoding='utf-8').read()
    except OSError:
        return names
    for code in IDENT.findall(text):
        for word in WORD.findall(code):
            if CAMEL.fullmatch(word):
                names.add(word)
    return names

LANGUAGE_MARKER = re.compile(r'<!-- doccheck: language (\w+) -->')

def doc_features(path):
    """Language features a tutorial asks for itself, with a `<!-- doccheck: language X -->` line:
    capture checking, for one whose examples write capture annotations."""
    return LANGUAGE_MARKER.findall(open(path, encoding='utf-8').read())

class Fence:
    def __init__(self, line, code):
        self.line = line          # markdown line of the fence's first code line
        self.code = code          # the code lines
        self.negatives = []       # (markdown line, code) lines that must be rejected

def fences(path):
    """The ```scala fences of a tutorial, in order, with `// does not compile` lines split out."""
    result = []
    lines = open(path, encoding='utf-8').read().split('\n')
    inside, start, buffer, skip = False, 0, [], False
    for number, line in enumerate(lines, 1):
        if not inside:
            if line.strip() == '<!-- doccheck: skip -->':
                skip = True
            elif FENCE_OPEN.match(line):
                inside, start, buffer = True, number + 1, []
                if skip:
                    skip = False
                    inside = False
                    # Consume the fence without recording it.
                    for later in lines[number:]:
                        number += 1
                        if FENCE_CLOSE.match(later):
                            break
                    continue
            continue
        if FENCE_CLOSE.match(line):
            fence = Fence(start, [])
            for offset, code in enumerate(buffer):
                if NEGATIVE.search(code):
                    fence.negatives.append((start + offset, NEGATIVE.split(code)[0].rstrip().rstrip('/').rstrip()))
                else:
                    fence.code.append(code)
            # Drop leading and trailing blank lines, keep interior ones (they may sit inside a definition).
            while fence.code and not fence.code[0].strip():
                fence.code.pop(0); fence.line += 1
            while fence.code and not fence.code[-1].strip():
                fence.code.pop()
            if fence.code or fence.negatives:
                result.append(fence)
            inside = False
        else:
            buffer.append(line)
    if inside:
        raise SystemExit(f'{path}:{start}: unterminated ```scala fence')
    return result

class Server:
    """A `flame serve` process on `port`, started and stopped here.

    flame runs as an Ethereal daemon: the `flame serve` process is a thin client of a JVM that
    outlives it. Sessions are never released, and each one holds a compiler over the whole
    release, so a long run exhausts the daemon's heap; `stop` therefore ends the daemon too, and
    the driver restarts the pair every few tutorials."""
    def __init__(self, port, verbose):
        self.port, self.verbose, self.process = port, verbose, None
        self.base = f'http://localhost:{port}'

    def up(self):
        try:
            with socket.create_connection(('localhost', self.port), timeout=0.5):
                return True
        except OSError:
            return False

    def start(self):
        if self.up():
            raise SystemExit(f'port {self.port} is already in use; pass --port to choose another')
        log = open('/tmp/doccheck-flame-serve.log', 'a')
        try:
            self.process = subprocess.Popen([FLAME, 'serve', '--port', str(self.port)], stdout=log, stderr=log,
                                            stdin=subprocess.DEVNULL)
        except OSError as error:
            raise SystemExit(f'cannot start {FLAME}: {error} (set FLAME=/path/to/flame)')
        for _ in range(120):
            if self.up():
                return
            time.sleep(0.5)
        raise SystemExit('flame serve did not come up; see /tmp/doccheck-flame-serve.log')

    def stop(self):
        if self.process:
            self.process.terminate()
            try:
                self.process.wait(10)
            except subprocess.TimeoutExpired:
                self.process.kill()
            self.process = None
        # The daemon JVM behind the client, identified by the name Ethereal gives it.
        subprocess.run(['pkill', '-f', 'ethereal.name=flame'], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        for _ in range(20):
            if not self.up():
                break
            time.sleep(0.5)
        time.sleep(1)

    def post(self, path, body=None, timeout=600):
        data = json.dumps(body).encode() if body is not None else b''
        request = urllib.request.Request(self.base + path, data=data, method='POST',
                                         headers={'Content-Type': 'application/json'})
        try:
            with urllib.request.urlopen(request, timeout=timeout) as response:
                return json.load(response)
        except urllib.error.HTTPError as error:
            return {'status': 'error', 'diagnostics': f'HTTP {error.code}: {error.read().decode(errors="replace")}'}
        except (TimeoutError, socket.timeout, urllib.error.URLError) as error:
            if 'timed out' in str(error).lower() or isinstance(error, (TimeoutError, socket.timeout)):
                return {'status': 'hung', 'diagnostics': f'no reply within {timeout}s'}
            raise

    def session(self):
        return self.post('/api/sessions')['session']

    def eval(self, session, code, timeout=600):
        reply = self.post(f'/api/sessions/{session}/eval', {'code': code}, timeout)
        for key in ('value', 'tpe', 'output', 'name', 'diagnostics', 'base'):
            reply[key] = ANSI.sub('', reply.get(key, '') or '')
        return reply

def classify(reply, tolerated):
    """OK, THREW, TOLERATED, UNDEFINED or STALE for a submission that was expected to run."""
    if reply['status'] == 'ran':
        return 'OK'
    diagnostics = reply['diagnostics']
    # A runtime exception is reported with the captured output and a stack trace.
    if '\n\tat ' in diagnostics or diagnostics.lstrip().startswith(('java.', 'scala.', 'Exception')):
        return 'THREW'
    for word in WORD.findall(diagnostics):
        if word in tolerated:
            return 'TOLERATED'
    # A fence whose only faults are names it never introduced — a `path`, a `Person` the prose
    # assumed — is incomplete rather than wrong about the API; it needs a definition or a fixture.
    notices = [n.strip() for n in diagnostics.split('; ') if n.strip()]
    if notices and all(n.startswith('Not found:') for n in notices):
        return 'UNDEFINED'
    return 'STALE'

FULL = False
OLDER = {}

def summarise(reply):
    """The first meaningful line of a reply's diagnostics (or output), for the report — or all of
    it under --full, indented beneath the verdict."""
    text = reply['diagnostics'].strip() or reply['output'].strip()
    if not text:
        return ''
    if FULL:
        return '\n' + '\n'.join('        ' + line for line in text.split('\n'))
    return text.split('\n')[0][:160]

def prepare(server, path, jars, args, replay):
    """A fresh session for `path`: settings, the release's jars, the fixture preamble, then the
    submissions in `replay` (fences that already ran before a hang), in that order."""
    name = os.path.basename(path)[:-3]
    session = server.session()
    # The language features Soundness itself compiles with (build.mill's `settings.scalaOptions`),
    # as far as flame offers them: an example using `erased`, a bounded numeric literal or a
    # `tracked` parameter needs them switched on in the session.
    features = ['erasedDefinitions', 'genericNumberLiterals', 'modularity'] + args.language + doc_features(path)
    for setting in ['/set experimental'] + [f'/set {s}' for s in args.set] + [f'/language {f}' for f in features]:
        server.eval(session, setting)
    started = time.time()
    for jar in jars:
        reply = server.eval(session, f'/classload {jar}')
        if reply['status'] != 'ran':
            print(f'{path}: classload of {jar} failed: {summarise(reply)}', file=sys.stderr)
    if jars and args.verbose:
        print(f'{path}: loaded {len(jars)} jars in {time.time() - started:.1f}s', file=sys.stderr)
    fixture = os.path.join('doc', 'fixtures', name + '.scala')
    fixture_reply = None
    if os.path.exists(fixture):
        lines = open(fixture, encoding='utf-8').read().split('\n')
        fixture_reply = server.eval(session, '\n'.join(lines))
        if fixture_reply['status'] != 'ran':
            # Like a fence: keep the imports that resolve (shimmed to the release's names where
            # they were renamed since), then try the rest of the fixture without them.
            for line in lines:
                if line.startswith('import '):
                    resubmit_import(server, session, line, replay, args.timeout)
            # Then each blank-line-separated definition on its own, so one that needs a given the
            # release lacks does not take the others down with it.
            rest = '\n'.join(l for l in lines if not l.startswith('import '))
            failed = None
            for block in re.split(r'\n\s*\n', rest):
                if block.strip():
                    retry = server.eval(session, block, args.timeout)
                    if retry['status'] != 'ran' and failed is None:
                        failed = retry
            fixture_reply = failed or {'status': 'ran', 'output': '', 'value': '', 'diagnostics': []}
    for code in replay:
        server.eval(session, code, args.timeout)
    return session, fixture_reply

def resubmit_import(server, session, line, replay, timeout):
    """Submit one import on its own, falling back to the release's name for a given renamed
    since; a resolving form is added to `replay` so a restarted daemon gets it too."""
    if server.eval(session, line, timeout)['status'] == 'ran':
        replay.append(line)
        return
    m = re.match(r'import ([a-z]\w*)\.([A-Za-z]\w*(?:\.\*)?)$', line.strip())
    older = m and (OLDER.get((m.group(1), m.group(2))) or OLDER.get(('*', m.group(2))))
    family = m and OLDER.get((m.group(1), '*'))
    shim = None
    if older and older.startswith('='):
        shim = f'import {older[1:]}'
    elif older:
        shim = f'import {m.group(1)}.{older}'
    elif family:
        shim = f'import {family}.{m.group(2)}'
    if shim and server.eval(session, shim, timeout)['status'] == 'ran':
        replay.append(shim)

def check(server, path, jars, tolerated, args):
    counts = {}
    def report(line, verdict, detail=''):
        counts[verdict] = counts.get(verdict, 0) + 1
        if verdict != 'OK' or args.verbose:
            print(f'{path}:{line}: {verdict}' + (f': {detail}' if detail else ''))
    replay = []
    session, fixture_reply = prepare(server, path, jars, args, replay)
    if fixture_reply is not None:
        ok = fixture_reply['status'] == 'ran'
        report(0, 'OK' if ok else 'FIXTURE-FAILED', '' if ok else summarise(fixture_reply))
    for fence in fences(path):
        if fence.code:
            code = '\n'.join(fence.code)
            reply = server.eval(session, code, args.timeout)
            if reply['status'] == 'hung':
                report(fence.line, 'HUNG', reply['diagnostics'])
                session, _ = prepare(server, path, jars, args, replay)
                continue
            if reply['status'] == 'ran':
                replay.append(code)
            else:
                # A rejected submission is discarded whole, imports included, so every later fence
                # would fail for want of them. Resubmit the fence's imports on their own, so the
                # ones that resolve stay in scope and the report shows the fence's real fault.
                for line in fence.code:
                    if line.startswith('import '):
                        resubmit_import(server, session, line, replay, args.timeout)
            verdict = classify(reply, tolerated)
            report(fence.line, verdict, summarise(reply) if verdict != 'OK' else '')
            if args.values and reply['status'] == 'ran' and (reply['value'] or reply['output']):
                shown = reply['output'].rstrip()
                if reply['value']:
                    shown = (shown + '\n' if shown else '') + f"{reply['name']} = {reply['value']}: {reply['tpe']}"
                for line in shown.split('\n'):
                    print(f'{path}:{fence.line}:     {line}')
        for line, code in fence.negatives:
            reply = server.eval(session, code, args.timeout)
            if reply['status'] == 'error':
                report(line, 'OK')
            else:
                report(line, 'NEGATIVE-FAILED', f'documented as not compiling, but it ran: {code.strip()}')
    return counts

def main():
    sys.stdout.reconfigure(line_buffering=True)
    global FULL, OLDER
    parser = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    parser.add_argument('docs', nargs='*', help='tutorial names (json) or paths; default: all of doc/modules')
    parser.add_argument('--port', type=int, default=8765)
    parser.add_argument('--release', help='the Soundness release flame is built against (default: the most-populated version in ~/.ivy2/local)')
    parser.add_argument('--values', action='store_true', help='print each fence\'s printed output, value and type')
    parser.add_argument('--verbose', '-v', action='store_true', help='report OK fences too')
    parser.add_argument('--full', action='store_true', help='print every line of a failing fence\'s diagnostics')
    parser.add_argument('--no-classload', action='store_true', help='use only the components flame bundles')
    parser.add_argument('--set', action='append', default=[], metavar='SETTING', help='extra `/set` (e.g. explain)')
    parser.add_argument('--language', action='append', default=[], metavar='FEATURE', help='extra `/language` feature (e.g. namedTuples)')
    parser.add_argument('--keep', action='store_true', help='leave the flame server running afterwards')
    parser.add_argument('--batch', type=int, default=4, help='tutorials per flame daemon before it is restarted')
    parser.add_argument('--timeout', type=int, default=120, help='seconds to wait for one fence before reporting it HUNG')
    args = parser.parse_args()
    FULL = args.full
    OLDER = release_names()

    docs = [d if d.endswith('.md') else os.path.join('doc', 'modules', d + '.md') for d in args.docs] \
        or sorted(glob.glob('doc/modules/*.md'))
    for doc in docs:
        if not os.path.exists(doc):
            raise SystemExit(f'no such tutorial: {doc}')

    version = args.release or release_version()
    jars = [] if args.no_classload or not version else component_jars(version)
    if version:
        print(f'checking against Soundness {version} (flame\'s release); {len(jars)} component jars', file=sys.stderr)
    tolerated = migration_names()

    server = Server(args.port, args.verbose)
    server.start()
    totals = {}
    try:
        for index, doc in enumerate(docs):
            if index and index % args.batch == 0:
                server.stop()
                server.start()
            counts = check(server, doc, jars, tolerated, args)
            for verdict, count in counts.items():
                totals[verdict] = totals.get(verdict, 0) + count
            summary = ', '.join(f'{count} {verdict}' for verdict, count in sorted(counts.items()))
            print(f'{doc}: {summary}', file=sys.stderr)
    finally:
        if not args.keep:
            server.stop()
    print('total: ' + ', '.join(f'{count} {verdict}' for verdict, count in sorted(totals.items())), file=sys.stderr)
    bad = sum(count for verdict, count in totals.items() if verdict not in ('OK', 'TOLERATED'))
    sys.exit(1 if bad else 0)

if __name__ == '__main__':
    main()
