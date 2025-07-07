import re, sys

# Like enable_cc.py, but toggles settings.cc(...) on an arbitrary submodule target,
# e.g. `python3 rep/enable_cc_target.py anthology.bundle`.
top, sub = sys.argv[1].split('.')
path = 'build.mill'
lines = open(path).read().split('\n')
n = len(lines)
cur = None
out = None
for idx, line in enumerate(lines):
    m = re.match(r'^object (\w+) extends ', line)
    if m: cur = m.group(1)
    if cur == top and re.match(rf'^  object {sub}\b', line):
        opens = line.count('(') - line.count(')')
        end = idx
        while opens > 0 and end+1 < n:
            end += 1
            opens += lines[end].count('(') - lines[end].count(')')
        bend = end+1
        while bend < n and not re.match(r'^  object ', lines[bend]) and not re.match(r'^object ', lines[bend]):
            bend += 1
        so = [i for i in range(end+1, bend) if 'def scalacOptions' in lines[i]]
        if so:
            rhs = so[0]+1
            r = lines[rhs]
            indent = re.match(r'^(\s*)', r).group(1)
            body = r.strip()
            if 'settings.cc(' not in body:
                lines[rhs] = f'{indent}settings.cc({body})'
                out = f"wrapped existing scalacOptions at line {rhs+1}"
            else:
                out = "already cc"
        else:
            sig = lines[end]
            if not sig.rstrip().endswith(':'):
                lines[end] = sig.rstrip() + ':'
            ins = ['    override def scalacOptions = Task:',
                   '      settings.cc(super.scalacOptions())']
            lines[end+1:end+1] = ins
            out = f"inserted override after line {end+1}"
        break
open(path,'w').write('\n'.join(lines))
print(f"{top}.{sub}: {out}")
