import re, sys
mod = sys.argv[1]
path = 'build.mill'
lines = open(path).read().split('\n')
n = len(lines)
cur = None
out = None
for idx, line in enumerate(lines):
    m = re.match(r'^object (\w+) extends ', line)
    if m: cur = m.group(1)
    if cur == mod and re.match(r'^  object test extends Tests', line):
        # find signature end (balance parens)
        opens = line.count('(') - line.count(')')
        end = idx
        while opens > 0 and end+1 < n:
            end += 1
            opens += lines[end].count('(') - lines[end].count(')')
        # block extent: until next object at indent<=2
        bend = end+1
        while bend < n and not re.match(r'^  object ', lines[bend]) and not re.match(r'^object ', lines[bend]):
            bend += 1
        block = lines[end+1:bend]
        # does block contain scalacOptions override?
        so = [i for i in range(end+1, bend) if 'def scalacOptions' in lines[i]]
        if so:
            # wrap the RHS line (the line after 'def scalacOptions = Task:') with settings.cc(...)
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
            # need to insert new override after signature line `end`
            sig = lines[end]
            if not sig.rstrip().endswith(':'):
                lines[end] = sig.rstrip() + ':'
            ins = ['    override def scalacOptions = Task:',
                   '      settings.cc(super.scalacOptions())']
            lines[end+1:end+1] = ins
            out = f"inserted override after line {end+1}"
        break
open(path,'w').write('\n'.join(lines))
print(f"{mod}: {out}")
