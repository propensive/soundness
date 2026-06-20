#!/usr/bin/env python3
"""Identify public-API reduction candidates across the Soundness monorepo.

Read-only. Parses the `soundness_*` re-export files (the authoritative public
set), joins to per-type declaration files (SN-847: one top-level type per
`module.TypeName.scala`), tags each candidate C1..C4, scores external usage,
and emits a categorised Markdown report.
"""
import os, re, subprocess, collections

ROOT = "/Users/propensive/work/worktrees/soundness/gondor/soundness"
LIB = os.path.join(ROOT, "lib")

# ---- 1. exported public names, per module --------------------------------
exported = collections.defaultdict(set)        # module -> {identifier}
export_owner = collections.defaultdict(set)     # identifier -> {module}
for mod in sorted(os.listdir(LIB)):
    for dirpath, _, files in os.walk(os.path.join(LIB, mod, "src")):
        for f in files:
            if not f.startswith("soundness_"):
                continue
            text = open(os.path.join(dirpath, f)).read()
            i = text.find("package soundness")
            if i < 0:
                continue
            body = text[i:]
            body = re.sub(r"//[^\n]*", "", body)          # strip line comments
            # capture every { ... } group that follows an `export`
            for m in re.finditer(r"export\b(.*?)\{(.*?)\}", body, re.S):
                for tok in re.split(r"[\s,]+", m.group(2)):
                    tok = tok.strip()
                    if re.fullmatch(r"[A-Za-z][A-Za-z0-9]*", tok or ""):
                        exported[mod].add(tok)
                        export_owner[tok].add(mod)

# ---- 2. declarations from type filenames ---------------------------------
# decls[name] = list of (module, sub, path, kind, is_test)
decls = collections.defaultdict(list)
declfile = re.compile(r"^[a-z][a-zA-Z0-9]*\.([A-Z][A-Za-z0-9]*)\.scala$")
for mod in sorted(os.listdir(LIB)):
    base = os.path.join(LIB, mod, "src")
    if not os.path.isdir(base):
        continue
    for dirpath, _, files in os.walk(base):
        sub = os.path.relpath(dirpath, base).split(os.sep)[0]
        is_test = "/test" in dirpath or dirpath.endswith("/test")
        for f in files:
            mm = declfile.match(f)
            if not mm:
                continue
            name = mm.group(1)
            path = os.path.join(dirpath, f)
            kind = "?"
            for line in open(path):
                k = re.match(r"\s*(?:transparent\s+)?(?:open\s+)?(?:sealed\s+)?"
                             r"(?:abstract\s+)?(?:case\s+)?(class|trait|enum|object|type)\b",
                             line)
                if k:
                    kind = k.group(1); break
            decls[name].append((mod, sub, path, kind, is_test))

# ---- helpers -------------------------------------------------------------
ABBREV_OK = {"Json","Yaml","Cbor","Tel","Dsv","Url","Uri","Http","Https","Css","Xml","Html",
             "Io","Uuid","Tcp","Udp","Dns","Pem","Aes","Des","Rsa","Dsa","Hmac","Rc2","Cose",
             "Mac","Iv","Tls","Ssl","Api","Cli","Tui","Ast","Dom","Svg","Png","Jpeg","Gif",
             "Ip","Ipv4","Ipv6","Mime","Utf","Ascii","Sql","Jdbc","Jvm","Os","Cpu","Id","Db"}
ABBREV_BAD = {"Err","Repr","Ctx","Cfg","Idx","Buf","Cmd","Env","Arg","Args","Param","Params",
              "Expr","Decl","Defn","Refl","Impl","Op","AddOp","Ref"}
ROLE_SUFFIX = ("Printer","Serializer","Emitter","Writer","Parser","Reader","Tokenizer",
               "Encoder","Decoder","Renderer","Builder","Factory","Handler","Provider",
               "Derivation","Derivable")
DERIVE_RE = re.compile(r"\bextends\s+\w*(Derivable|ProductDerivable|Derivation|ProductDerivation)\b")

def humps(name):
    return re.findall(r"[A-Z][a-z0-9]*", name)

def external_usage(name, owner_mod):
    """count files outside owner_mod (non-test) referencing the bare name"""
    try:
        out = subprocess.run(
            ["grep","-rlw","--include=*.scala", name, LIB],
            capture_output=True, text=True).stdout
    except Exception:
        return -1
    files = [p for p in out.splitlines()
             if p and "/test/" not in p
             and not os.path.basename(p).startswith("soundness_")
             and f"/lib/{owner_mod}/" not in p]
    return len(files)

# ---- 3/4. tag + score ----------------------------------------------------
all_exported = set().union(*exported.values()) if exported else set()

c1, c2, c3, c4h, c4s = [], [], [], [], []

# C4-homonym: bare name declared top-level (non-test) in >=2 modules
homonyms = {}
for name, rows in decls.items():
    mods = sorted({m for (m,s,p,k,t) in rows if not t})
    if len(mods) >= 2 and name[0].isupper():
        homonyms[name] = mods

# role-suffix synonym families
synfam = collections.defaultdict(list)

for name, rows in sorted(decls.items()):
    nontest = [r for r in rows if not r[4]]
    if not nontest:
        continue
    mod, sub, path, kind, _ = nontest[0]
    is_pub = name in all_exported
    hp = humps(name)

    # C3 — typeclass-backing: top-level role-suffix engine types only
    # (format-marker types like Json/Xml merely *contain* nested derivations —
    #  those are scanned separately below, not flagged here)
    is_engine = name.endswith(ROLE_SUFFIX[:13])  # serialization/parsing role suffixes
    is_derive = name.endswith(("Derivation","Derivable"))
    if is_engine or is_derive:
        c3.append((name, mod, kind, is_pub, path))
        for suf in ROLE_SUFFIX:
            if name.endswith(suf) and len(name) > len(suf):
                synfam[suf].append((name, mod))

    # C2 — abbreviations
    if name in ABBREV_BAD or any(h in ABBREV_BAD for h in hp):
        c2.append((name, mod, kind, is_pub))

    # C1 — multi-word; pick the LONGEST hump-prefix that is itself a declared
    # top-level type in the same module (best nesting target), else first hump.
    if len(hp) >= 2 and not is_derive and not is_engine:
        def declared_here(p):
            return (p in decls) and any((m == mod and not t) for (m,s,pp,k,t) in decls[p])
        best = None
        for j in range(len(hp)-1, 0, -1):
            cand = "".join(hp[:j])
            if declared_here(cand):
                best = cand; break
        prefix = best or hp[0]
        prefix_is_ns = best is not None
        prefix_known = prefix in ABBREV_OK or prefix_is_ns
        c1.append((name, mod, kind, is_pub, prefix, prefix_is_ns, prefix_known))

for name, mods in sorted(homonyms.items()):
    c4h.append((name, mods))

# nested derivation objects: `object XxxDerivation extends Derivable/...`
# These exist only to back a typeclass and are summoned generically — never
# named by users. Scan all non-test sources.
nested_deriv = []  # (objname, module, relpath)
deriv_obj = re.compile(r"\bobject\s+([A-Z][A-Za-z0-9]*)\s+extends\s+\w*"
                       r"(?:Derivable|Derivation)\b")
for mod in sorted(os.listdir(LIB)):
    base = os.path.join(LIB, mod, "src")
    if not os.path.isdir(base):
        continue
    for dp, _, fs in os.walk(base):
        if "/test" in dp:
            continue
        for f in fs:
            if not f.endswith(".scala"):
                continue
            p = os.path.join(dp, f)
            for m in deriv_obj.finditer(open(p).read()):
                nested_deriv.append((m.group(1), mod, os.path.relpath(p, ROOT)))

# ---- 5. emit report ------------------------------------------------------
def yn(b): return "exported" if b else "internal"

lines = []
A = lines.append
A("# Public API surface — reduction candidate inventory\n")
A("Generated read-only from the `soundness_*` re-export files and "
  "`module.TypeName.scala` declarations. Categories C1–C4 per the plan. "
  "**Identification only** — no fixes applied here.\n")
A(f"- modules scanned: {len(os.listdir(LIB))}")
A(f"- distinct public identifiers exported into `soundness`: {len(all_exported)}")
A(f"- top-level type declarations (incl. tests): {len(decls)}\n")
A("\n**Reading the tables (heuristic output — confirm before acting):**")
A("- C1 `→ suggested` nests under the *longest* same-module type that prefixes the name; "
  "a few split points still need a human eye (e.g. `Git.RefError` could be `GitRef.Error`).")
A("- C3b lists every `object … extends …Derivable/Derivation`. Those suffixed `*Derivation` "
  "are pure backing instances (demote/anonymise). Those named like a public typeclass "
  "(`Spannable`, `Tabulable`, `Digestible`, `JsonSchema`, …) *are* the typeclass companion "
  "self-deriving — keep the type, the note just flags where derivation lives.")
A("- 'exported' = appears in a `soundness_*` re-export; 'internal' = compiles but not in the "
  "`soundness` namespace (already invisible to `import soundness.*`).\n")

# C4-homonym first (cross-cutting)
A("\n## C4-homonym — one name, two meanings (declared in ≥2 modules)\n")
A("| name | modules | both/which exported |")
A("|---|---|---|")
for name, mods in c4h:
    ex = [m for m in mods if name in exported.get(m,set())]
    note = "both exported ⚠" if len(ex)>=2 else (f"only {ex[0]} exported" if ex else "neither exported")
    A(f"| `{name}` | {', '.join(mods)} | {note} |")

A("\n## C4-synonym — competing terms for one role (role-suffix families)\n")
for suf in sorted(synfam):
    items = sorted(set(synfam[suf]))
    A(f"- **`*{suf}`** ({len(items)}): " + ", ".join(f"`{n}`({m})" for n,m in items))

# C3
A("\n## C3 — typeclass-backing entities (candidates to demote/inline/de-export)\n")
A("| name | module | kind | visibility |")
A("|---|---|---|---|")
for name, mod, kind, pub, path in sorted(c3, key=lambda r:(not r[3], r[1])):
    A(f"| `{name}` | {mod} | {kind} | {yn(pub)} |")

# C2
A("\n## C3b — nested derivation objects (generically summoned; need not be named)\n")
A("These `object …Derivation extends Derivable/…` instances back a typeclass "
  "and are reached only via derivation — candidates to make anonymous/private.\n")
A("| object | module | file |")
A("|---|---|---|")
for objn, mod, rel in sorted(set(nested_deriv)):
    A(f"| `{objn}` | {mod} | `{rel}` |")

A("\n## C2 — non-established abbreviations\n")
A("| name | module | kind | visibility |")
A("|---|---|---|---|")
for name, mod, kind, pub in sorted(set(c2)):
    A(f"| `{name}` | {mod} | {kind} | {yn(pub)} |")

# C1 — only exported + prefix-is-a-real-namespace are the strong candidates
A("\n## C1 — multi-word names → `Foo.Bar` (strong: prefix already names a same-module type)\n")
A("| name | module | → suggested | kind | visibility | prefix is local namespace |")
A("|---|---|---|---|---|---|")
strong = [r for r in c1 if r[3] and r[5]]   # exported & prefix_is_ns
for name, mod, kind, pub, prefix, pis, pk in sorted(strong, key=lambda r:r[1]):
    rest = name[len(prefix):]
    A(f"| `{name}` | {mod} | `{prefix}.{rest}` | {kind} | {yn(pub)} | yes |")
A(f"\n_({len(strong)} strong; {len([r for r in c1 if r[3]])} total exported multi-word)_\n")

A("\n## Pilot (enigmatic) — done, and gotchas for the fix step\n")
A("Executed as the reference transformation: `PemError → Pem.Error`, `PemLabel → Pem.Label` "
  "(folded into the existing `object Pem`, the two `enigmatic.Pem{Error,Label}.scala` files "
  "deleted, both names dropped from the `soundness` export). `enigmatic.{core,cose,openssl,test}` "
  "compile clean; all 51 tests pass. Net: two fewer top-level names in `soundness`.\n")
A("Gotchas observed (apply to every C1 rename):")
A("- **`XxxError → Xxx.Error` shadows the base.** A nested `case class Error` makes the bare "
  "`Error` in its own `extends` clause self-referential — qualify the base as "
  "`extends fulminate.Error(...)`. This bites *every* error-nesting (the largest C1 sub-cluster).")
A("- **Move imports with the body.** Nesting `PemLabel` pulled `spectacular.*` (its `Showable`) "
  "into `Pem.scala`; check the donor file's imports when folding a type in.")
A("- **SN-847**: deleting the old `module.Xxx.scala` file is required (the type is no longer "
  "top-level); the receiving `module.Foo.scala` already satisfies the rule. SN-398 ordering "
  "(companion `object` before its `class`/`enum`) must hold for the *nested* pair too.")
A("- **Enum cases** (`Proprietary`, `fromOrdinal`) resolve unqualified inside the nested "
  "companion object exactly as they did at top level — no change needed there.\n")

open(os.path.join(ROOT,"doc","api-reduction-candidates.md"),"w").write("\n".join(lines))
print("wrote doc/api-reduction-candidates.md")
print("C1 strong:", len(strong), "C2:", len(set(c2)), "C3:", len(c3),
      "C4-homonym:", len(c4h), "C4-syn families:", len(synfam))
