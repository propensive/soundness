#!/usr/bin/env python3
"""Find coercion helpers — small defs that only re-wrap a value — and rank them.

The standard is `doc/standards/plumbing.md`. This script gathers candidates with a regex pass,
attaches the signals it can compute itself (kind, call sites, duplicates), sends them in batches
to a language model with the rubric section of the standard as the prompt, recomputes each score
from the booleans the model returns, and writes `etc/plumbing-ranked.tsv`, worst first.

  --dry-run            list the gated candidates without calling the model
  --runner batch|realtime|cli
                       Message Batches API (default, half price), synchronous API calls, or
                       the `claude -p` CLI (no API key; runs on a subscription)
  --model ID           default claude-haiku-4-5 (CLI runner: an alias such as `haiku`)
  --ids FILE           score only the candidate ids listed in FILE (one per line)
  --sample N           print N candidate ids, stratified by kind, for hand-labelling
  --totals             print the band counts from the committed census and exit
  --json               also write the raw model responses next to the census

Responses are cached by content under `etc/.plumbing-cache/`, so a re-run over unchanged
candidates makes no requests. The anthropic SDK is needed for the API runners:
`~/.cache/soundness/plumbing-venv/bin/python etc/find-plumbing.py`, after
`python3 -m venv ~/.cache/soundness/plumbing-venv && …/pip install anthropic`.
"""

import glob, hashlib, json, os, re, subprocess, sys, time

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
STANDARD = 'doc/standards/plumbing.md'
CENSUS = 'etc/plumbing-ranked.tsv'
CACHE = 'etc/.plumbing-cache'
SKIP_DIRS = ('/src/test/', '/src/bench/', 'lib/proscenium/', 'lib/anticipation/src/text/')
BATCH_SIZE = 20
MAX_BODY_LINES = 12

WEIGHTS = {'S1': 2, 'S2': 3, 'S3': 3, 'S4': 1, 'S5': 2, 'S6': 3, 'S7': 2, 'S8': 1, 'S9': 1,
           'S10': 3, 'X1': -3, 'X2': -3, 'X3': -2, 'X4': -2}

REPLACEMENTS = {'.tt', '.s', '.show', '.as[T]', '.in[Text]', '.in[Data]', '.to[List]', '.to[Map]',
                '.stdlib', '.puncture', '.optional', 'safely', 'Enumerable', 'Extractable',
                'Conversion→method', 'extension', 'inline at call site', 'none'}

# Same stripping as etc/check-while-count.py — a `t"toString"` or a comment must not gate a def —
# except that newlines inside a stripped span are kept, so line numbers still match the source.
def blank(replacement):
  return lambda match: replacement + '\n' * match.group(0).count('\n')

STRIP = [
  (re.compile(r'"""(?:.|\n)*?"""'), blank('""')),
  (re.compile(r'"(?:[^"\\\n]|\\.)*"'), blank('""')),
  (re.compile(r'/\*(?:.|\n)*?\*/'), blank('')),
  (re.compile(r'//[^\n]*'), blank('')),
]

DEF = re.compile(
  r'^(?P<indent>\s*)(?P<mods>(?:(?:private|protected|override|inline|transparent|final)\s+)*)'
  r'def\s+(?P<name>[A-Za-z_][A-Za-z0-9_]*)(?:\[[^\]]*\])?\((?P<params>[^)]*)\)'
  r'(?P<more>(?:\([^)]*\))*)\s*(?::\s*(?P<result>[^=]+?))?\s*=\s*(?P<rest>.*)$')
CONVERSION = re.compile(
  r'^(?P<indent>\s*)(?P<mods>(?:(?:private|protected|inline)\s+)*)given\s+(?:(?P<name>\w+)\s*:\s*)?'
  r'(?:.*\b)?Conversion\[(?P<params>[^\]]*)\]\s*=\s*(?P<rest>.*)$')
ENCLOSING = re.compile(r'^\s*(?:(?:private|protected|override|inline|transparent|final)\s+)*'
                       r'(?P<kw>def|extension|object|class|trait|enum|given|package|case)\b')

VERB = re.compile(r'^(?:to|from|as|make|mk|convert|lift|wrap|unwrap)[A-Z]|^(?:text|string|bytes|number|decimal|hex|key|node)$')
CODEC = re.compile(r'^(?:read|write|parse|decode|encode|serialize|deserialize|apply|unapply)')
ADAPTER = re.compile(r'\.(?:tt|s|nn|toString|toInt|toLong|toDouble|toByte|toChar|toList|toMap|toSeq|toArray|getBytes|stdlib|to)\b'
                     r'|\bText\(|\bArray\.unsafe(?:Frozen|Jvm)\(|\basInstanceOf\b')
LADDER_CASE = re.compile(r'^\s*case\s+[^=]+=>\s*[A-Z][\w.]*(?:\([^()]*\))?\s*$')


def code(source):
  for pattern, replacement in STRIP: source = pattern.sub(replacement, source)
  return source


def sources():
  for path in sorted(glob.glob('lib/*/src/*/**/*.scala', recursive=True)):
    if not any(skip in path for skip in SKIP_DIRS): yield path


def indent_of(line): return len(line) - len(line.lstrip(' '))


def extent(lines, start):
  """Lines of the definition starting at `start`: the signature line plus every following line
  indented deeper than it, blank lines included, capped at MAX_BODY_LINES+1."""
  base = indent_of(lines[start])
  end = start + 1
  while end < len(lines) and (not lines[end].strip() or indent_of(lines[end]) > base): end += 1
  while end > start + 1 and not lines[end - 1].strip(): end -= 1
  return lines[start:min(end, start + MAX_BODY_LINES + 1)], end - start - 1


def enclosing(lines, start):
  """The keyword of the nearest less-indented construct above `start`, walking outwards."""
  base = indent_of(lines[start])
  for index in range(start - 1, -1, -1):
    line = lines[index]
    if not line.strip() or indent_of(line) >= base: continue
    match = ENCLOSING.match(line)
    if match: return match.group('kw'), line.strip()
    base = indent_of(line)
  return None, ''


def kind_of(mods, enclosing_kw):
  if enclosing_kw == 'def': return 'local'
  if 'private' in mods: return 'private'
  if 'protected' in mods: return 'protected'
  return 'public'


def normalise(body, params):
  names = [p.split(':')[0].strip().split(' ')[-1] for p in params.split(',') if ':' in p]
  text = body
  for index, name in enumerate(names):
    if name: text = re.sub(r'\b' + re.escape(name) + r'\b', f'_p{index}', text)
  return re.sub(r'\s+', ' ', text).strip()


ROUND_TRIP = re.compile(r'\.s\b.*\.tt\b|\.toString\b.*\.tt\b|\.stdlib\b.*\.to[\[(]|unsafeJvm\b.*unsafeFrozen\b')
LOGIC = re.compile(r'=>|\bif\b|\bmatch\b|\bwhile\b|[-+*/%<>!]|&&|\|\|')


def gated(name, body_lines, is_conversion, kind):
  """A `given Conversion`; a short body built from adapters; a one-to-one ladder; a coercion verb
  on a short body. A public def is API, so it qualifies only as a Conversion, a round trip, or a
  one-line adapter chain with no logic in it — an instance method that happens to cast is not
  the target."""
  body = ' '.join(line.strip() for line in body_lines)
  if is_conversion: return True
  if re.search(r'\bmatch\s*$', body_lines[0] if body_lines else ''):
    cases = [line for line in body_lines[1:] if line.strip()]
    # One case may be a catch-all or a panic without the shape ceasing to be a ladder.
    if len(cases) >= 2 and sum(1 for line in cases if not LADDER_CASE.match(line)) <= 1: return True
  if kind == 'public':
    if ROUND_TRIP.search(body): return True
    return len(body_lines) == 1 and bool(ADAPTER.search(body)) and not LOGIC.search(body)
  if len(body_lines) <= 3 and VERB.search(name): return True
  if len(body_lines) <= 3 and ADAPTER.search(body): return True
  return False


def candidates():
  found = []
  for path in sources():
    raw = open(path).read()
    raw_lines = raw.split('\n')
    lines = code(raw).split('\n')
    for index, line in enumerate(lines):
      match = DEF.match(line)
      is_conversion = False
      if not match:
        match = CONVERSION.match(line)
        if not match: continue
        is_conversion = True
      mods = match.group('mods') or ''
      if 'override' in mods: continue
      name = match.group('name') or ('conversion' if is_conversion else None)
      if not name or CODEC.match(name): continue
      params = match.group('params') or ''
      more = match.groupdict().get('more') or ''
      if 'using ' in params or 'erased ' in params or 'using ' in more: continue
      block, body_count = extent(lines, index)
      if body_count > MAX_BODY_LINES: continue
      rest = match.group('rest').strip()
      body_lines = ([rest] if rest else []) + [l for l in block[1:]]
      body_text = ' '.join(l.strip() for l in body_lines)
      if not body_text or "'{" in body_text or 'Expr' in body_text: continue
      kw, signature = enclosing(lines, index)
      kind = kind_of(mods, kw)
      if not gated(name, body_lines, is_conversion, kind): continue
      norm = normalise(body_text, params)
      ident = f'{path}:{name}:' + hashlib.sha1(norm.encode()).hexdigest()[:8]
      while any(item['id'] == ident for item in found): ident += '~'
      if kind == 'local':
        scope_start = index
        while scope_start > 0 and not (indent_of(lines[scope_start]) < indent_of(line) and ENCLOSING.match(lines[scope_start]) and ENCLOSING.match(lines[scope_start]).group('kw') == 'def'):
          scope_start -= 1
        scope, _ = extent(lines, scope_start) if scope_start != index else (lines, 0)
        scope_text = '\n'.join(scope) if scope_start != index else '\n'.join(lines)
      else: scope_text = '\n'.join(lines)
      calls = len(re.findall(r'\b' + re.escape(name) + r'\b', scope_text)) - 1
      excerpt = raw_lines[max(0, index - 2):index + len(block)]
      if kind == 'local' and signature: excerpt = [signature + ' …'] + excerpt
      found.append({
        'id': ident, 'path': path, 'line': index + 1, 'kind': kind, 'name': name,
        'calls': max(calls, 0), 'oneliner': body_count == 0 and bool(rest),
        'norm': norm, 'excerpt': '\n'.join(excerpt), 'params': params,
      })
  by_norm = {}
  for item in found: by_norm.setdefault(item['norm'], []).append(item)
  for item in found:
    twins = [t for t in by_norm[item['norm']] if t['id'] != item['id']]
    item['dups'] = len(twins)
    item['twins'] = ';'.join(f"{t['path']}:{t['line']}" for t in twins[:4])
  return found


def driver_score(item):
  score = {'local': 2, 'private': 1}.get(item['kind'], 0)
  score += {1: 2, 2: 1}.get(item['calls'], -1 if item['calls'] >= 4 else 0)
  if item['dups']: score += 3
  if item['oneliner']: score += 1
  return score


def rubric():
  text = open(os.path.join(ROOT, STANDARD)).read()
  start, end = text.index('<!-- rubric:begin -->'), text.index('<!-- rubric:end -->')
  return text[start + len('<!-- rubric:begin -->'):end].strip()


def render(batch):
  parts = []
  for item in batch:
    header = f"### {item['id']}\nkind: {item['kind']}; call sites in scope: {item['calls']}; duplicates elsewhere: {item['dups']}"
    parts.append(header + '\n```scala\n' + item['excerpt'] + '\n```')
  return f'Score these {len(batch)} candidates.\n\n' + '\n\n'.join(parts)


SCHEMA = {
  'type': 'array',
  'items': {
    'type': 'object',
    'properties': {
      'id': {'type': 'string'},
      'signs': {'type': 'object',
                'properties': {key: {'type': 'boolean'} for key in WEIGHTS},
                'required': list(WEIGHTS), 'additionalProperties': False},
      'replacement': {'type': 'string'},
      'reason': {'type': 'string'},
    },
    'required': ['id', 'signs', 'replacement', 'reason'], 'additionalProperties': False,
  },
}


def cache_key(model, system, user):
  return hashlib.sha1((model + '\0' + system + '\0' + user).encode()).hexdigest()


def cached(key):
  path = os.path.join(ROOT, CACHE, key + '.json')
  return open(path).read() if os.path.exists(path) else None


def store(key, text):
  os.makedirs(os.path.join(ROOT, CACHE), exist_ok=True)
  open(os.path.join(ROOT, CACHE, key + '.json'), 'w').write(text)


def parse(text):
  text = text.strip()
  if text.startswith('```'): text = re.sub(r'^```(?:json)?\s*|\s*```$', '', text)
  start, end = text.find('['), text.rfind(']')
  return json.loads(text[start:end + 1])


def params_for(model, system, user):
  return dict(model=model, max_tokens=16000,
              system=[{'type': 'text', 'text': system, 'cache_control': {'type': 'ephemeral'}}],
              messages=[{'role': 'user', 'content': user}],
              output_config={'format': {'type': 'json_schema', 'schema': SCHEMA}})


def run_realtime(client, model, system, jobs):
  for key, user in jobs:
    response = client.messages.create(**params_for(model, system, user))
    store(key, ''.join(block.text for block in response.content if block.type == 'text'))


def run_batch(client, model, system, jobs):
  from anthropic.types.message_create_params import MessageCreateParamsNonStreaming
  from anthropic.types.messages.batch_create_params import Request
  requests = [Request(custom_id=key, params=MessageCreateParamsNonStreaming(**params_for(model, system, user)))
              for key, user in jobs]
  batch = client.messages.batches.create(requests=requests)
  print(f'batch {batch.id}: {len(requests)} requests', file=sys.stderr)
  while True:
    batch = client.messages.batches.retrieve(batch.id)
    if batch.processing_status == 'ended': break
    print(f'  {batch.request_counts.processing} processing…', file=sys.stderr)
    time.sleep(30)
  for result in client.messages.batches.results(batch.id):
    if result.result.type == 'succeeded':
      message = result.result.message
      store(result.custom_id, ''.join(block.text for block in message.content if block.type == 'text'))
    else: print(f'error: {result.custom_id}: {result.result.type}', file=sys.stderr)


def run_cli(model, system, jobs):
  for key, user in jobs:
    prompt = system + '\n\n' + user
    completed = subprocess.run(
      ['claude', '-p', '--model', model, '--output-format', 'json', '--no-session-persistence',
       '--disallowedTools', 'Bash,Read,Edit,Write,Glob,Grep,WebFetch,WebSearch,Agent'],
      input=prompt, capture_output=True, text=True, cwd='/')
    if completed.returncode != 0:
      print(f'error: claude exited {completed.returncode}: {completed.stderr[:200]}', file=sys.stderr)
      continue
    store(key, json.loads(completed.stdout)['result'])


def score(items, model, runner):
  system = rubric()
  batches = [items[i:i + BATCH_SIZE] for i in range(0, len(items), BATCH_SIZE)]
  jobs, keys = [], []
  for batch in batches:
    user = render(batch)
    key = cache_key(model, system, user)
    keys.append((key, batch))
    if cached(key) is None or '--fresh' in sys.argv: jobs.append((key, user))
  print(f'{len(items)} candidates in {len(batches)} batches; {len(jobs)} to request', file=sys.stderr)
  if jobs:
    if runner == 'cli': run_cli(model, system, jobs)
    else:
      import anthropic
      client = anthropic.Anthropic()
      (run_batch if runner == 'batch' else run_realtime)(client, model, system, jobs)
  results, raw = {}, {}
  for key, batch in keys:
    text = cached(key)
    if text is None: continue
    raw[key] = text
    try: entries = parse(text)
    except (ValueError, json.JSONDecodeError):
      print(f'error: unparseable response for batch {key}', file=sys.stderr)
      continue
    for entry in entries:
      if isinstance(entry, dict) and 'id' in entry: results[entry['id']] = entry
  return results, raw


def band(total):
  if total >= 10: return 'definite'
  if total >= 5: return 'likely'
  if total >= 0: return 'weak'
  return 'legitimate'


def write_census(items, results, census=CENSUS):
  rows = []
  for item in items:
    entry = results.get(item['id'])
    if not entry: continue
    signs = entry.get('signs', {})
    model_score = sum(WEIGHTS[key] for key, value in signs.items() if key in WEIGHTS and value)
    driver = driver_score(item)
    replacement = entry.get('replacement', 'none')
    if replacement not in REPLACEMENTS: replacement = '?' + replacement
    reason = re.sub(r'\s+', ' ', entry.get('reason', '')).strip()
    rows.append((model_score + driver, item, model_score, driver, replacement,
                 ''.join(key for key in WEIGHTS if signs.get(key)), reason))
  rows.sort(key=lambda row: (-row[0], row[1]['path'], row[1]['line']))
  with open(os.path.join(ROOT, census), 'w') as file:
    file.write('# id\tpath\tline\tkind\tname\tcalls\tdups\tmodel\tdriver\ttotal\tband\treplacement\tsigns\treason\n')
    for total, item, model_score, driver, replacement, signs, reason in rows:
      file.write('\t'.join(map(str, [item['id'], item['path'], item['line'], item['kind'], item['name'],
                                     item['calls'], item['dups'], model_score, driver, total, band(total),
                                     replacement, signs, reason])) + '\n')
  return rows


def totals():
  counts = {}
  path = os.path.join(ROOT, CENSUS)
  if not os.path.exists(path): return counts
  for line in open(path):
    if line.startswith('#') or not line.strip(): continue
    fields = line.rstrip('\n').split('\t')
    counts[fields[10]] = counts.get(fields[10], 0) + 1
  return counts


def option(flag, default=None):
  if flag in sys.argv:
    index = sys.argv.index(flag)
    return sys.argv[index + 1] if index + 1 < len(sys.argv) else default
  return default


def main():
  os.chdir(ROOT)
  if '--totals' in sys.argv:
    counts = totals()
    print('  '.join(f'{name}: {counts.get(name, 0)}' for name in ('definite', 'likely', 'weak', 'legitimate')))
    return
  items = candidates()
  if '--sample' in sys.argv:
    import random
    random.seed(int(option('--seed', '1')))
    n = int(option('--sample', '30'))
    kinds = {'local': [], 'private': [], 'ladder': []}
    for item in items:
      if item['name'] == 'conversion' or 'match' in item['norm']: kinds['ladder'].append(item)
      elif item['kind'] == 'local': kinds['local'].append(item)
      elif item['kind'] == 'private': kinds['private'].append(item)
    for name, pool in kinds.items():
      for item in random.sample(pool, min(len(pool), n // 3)): print(item['id'])
    return
  ids_file = option('--ids')
  if ids_file:
    wanted = {line.split('\t')[0].strip() for line in open(ids_file) if line.strip() and not line.startswith('#')}
    items = [item for item in items if item['id'] in wanted]
  limit = option('--limit')
  if limit: items = items[:int(limit)]
  if '--dry-run' in sys.argv:
    for item in items:
      print(f"{item['id']}\t{item['kind']}\tcalls={item['calls']}\tdups={item['dups']}\tdriver={driver_score(item)}")
      print('    ' + item['excerpt'].replace('\n', '\n    '))
    print(f'{len(items)} candidates', file=sys.stderr)
    return
  runner = option('--runner', 'batch')
  model = option('--model', 'haiku' if runner == 'cli' else 'claude-haiku-4-5')
  results, raw = score(items, model, runner)
  census = option('--out', CENSUS)
  rows = write_census(items, results, census)
  if '--json' in sys.argv:
    json.dump(raw, open(os.path.join(ROOT, 'etc/plumbing-raw.json'), 'w'), indent=1)
  counts = {}
  for total, *_ in rows: counts[band(total)] = counts.get(band(total), 0) + 1
  missing = len(items) - len(rows)
  print(f'wrote {census}: ' + '  '.join(f'{name} {counts.get(name, 0)}' for name in ('definite', 'likely', 'weak', 'legitimate'))
        + (f'  ({missing} unscored)' if missing else ''))


if __name__ == '__main__': main()
