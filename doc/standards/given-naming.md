# Contextual-Value Naming Standards

This standard defines how importable `given` values are named and grouped. It covers
the *orphan* contextual values: those a user brings into scope by name, either from a
choice package (`import strategies.throwUnsafely`) or, for the few that cannot be
anchored to a companion, from a library's top level (`import bitumen.tarPathOpenable`).
Where a given *lives* is decided by the placement rules in `.claude/CLAUDE.md`; this
document decides what it is *called*. For why the name matters at all, see
[declarative context](../philosophy/declarative-context.md) and
[naming](../philosophy/naming.md).

Givens in companion objects (`InlineAnchoring.default`, `Buffering.standard`) are outside
this standard: they resolve by type and are never written at an import site.

## The test

A reader who knows everything Soundness can do, but nothing about what it calls things,
should be able to say what a given does from its name alone, with the package stripped
off. `throwUnsafely` passes: it is a strategy, and it throws. `enabled` and `native` fail:
enabled *what*, and native *to what*? The name is the only thing visible at the import
line, and often the only thing visible anywhere, so it carries the whole meaning.

## Rules

### 1. The family is the role, as a package

A choice package is named for the role its members fill: a plural count noun
(`strategies`, `stdios`, `filesystemBackends`, `probates`) or a mass noun
(`formatting`, `threading`, `logging`). Never a singular count noun: `blockCipherModes`,
not `blockCipherMode`. Family names are global — same-named blocks from different
libraries merge in the `soundness` umbrella — so a family must mean one role
everywhere.

Families are never nested. A library declares each family as one top-level `package
<family>:` block, and the umbrella mirrors it under the same name, so
`import aviation.monthFormats.englishMonths` and `import soundness.monthFormats.englishMonths`
are the same path. A family that would naturally sit inside another takes its parent's word
as a prefix instead (`dateSeparators`, `timeSeparators`, `randomSizes`, `pathInterfaces`).

### 2. The name is the choice, then the role

The choice word comes first, and the role's singular is appended unless the choice
already implies it:

```scala
import probates.cancelProbate           // cancel + Probate
import textSanitizers.strictSanitizer   // strict + Sanitizer
import sortingAlgorithms.quicksort      // "quicksort" is already a sorting algorithm
import charEncoders.utf8Encoder         // UTF-8 could be a decoder; say which
import regexBackends.re2                // RE2 is a regex engine; nothing to add
```

The suffix is the role's own word (`…Probate`, `…Stdio`, `…Formatting`), not a synonym
for it, so that every member of a family ends the same way.

### 3. Unique with the package removed

Every orphan given is unique across the whole of Soundness, judged as if the package
were not there. All of them are exported into `soundness`, and any two may have to
coexist in one scope, so `import x.enabled` and `import y.enabled` cannot both exist.

The corollary is that there are no toggle members. A switch is named for the behaviour
it selects, on both sides:

```scala
import filesystemOptions.deleteRecursively     // not deleteRecursively.enabled
import filesystemOptions.deleteOnlyEmpty       // not deleteRecursively.disabled
import arithmeticOptions.checkedOverflow       // not overflow.checked
import dynamicAccess.dynamicJson               // not dynamicJsonAccess.enabled
```

### 4. Existing vocabulary only

A name reuses the type or term it selects, so that the import and the declaration it
enables read with the same words: `cancelProbate` selects a `Probate`; `taiChronometry`
selects the `Tai` timeline; `offHeapCloak` is the `OffHeapCloak`. A name never coins a
term that appears nowhere else in Soundness.

`default`, `standard`, `basic` and `simple` are not choice words: they say that a
choice is common, not what it is. `indentedCssFormatting`, `thickTableStyle` and
`timestampedLogFormat` each say what they do; `standardCssFormatting` said only that
somebody preferred it. The exception is when such a word *is* the type's own name
(`standardKeyboard` selects `Keyboard.Standard`; the RFC 4648 alphabets are called
"standard" by their specification).

Numerals belong in a name only when they are part of the choice (`sha256Signature`,
`xterm256Termcap`, `iso8601DateFormat`), never to tell two givens apart
(`populatedEquality2`).

### 5. Provenance words for backends

When the choice is *which implementation*, the choice word names where the
implementation comes from. One word per source, used the same way everywhere:

| Source | Word | Examples |
|---|---|---|
| A Java module | the module's name | `javaBaseFilesystem`, `javaBaseSockets`, `javaNetHttp`, `jdkHttpserver` |
| A WASI interface | `wasi` + the interface | `wasiFilesystem` (`wasi:filesystem`), `wasiHttp`, `wasiSockets` |
| Scala Native, over libc/POSIX | `scalaNative` | `scalaNativeFilesystem`, `scalaNativeSockets` |
| Scala.js | `javascript` | `javascriptThreading` |
| Soundness's own pure-Scala code | `soundness` | `soundnessHttp`, `soundnessHttpd`, `soundnessProvider` |
| A third party | its own name | `opensslProvider`, `re2`, `resendCourier` |

Java modules are named because that is what a `requires` clause names: Soundness
intends to declare, per module, which Java modules it depends on, and the given that
selects a Java-backed implementation should say which module it is drawing on.
`java.base` supplies most of them, so `javaBase…` is the common case; where two
`java.base` choices differ, the name says how (`fileDescriptorStdio` wraps the process
file descriptors; `javaLangSystemStdio` wraps `java.lang.System`'s redirectable
streams).

Two things look like provenance and are not. A Java *type* adopted as a representative
keeps the type-path name it already has elsewhere in Soundness (`javaNioPath`,
`javaUtilDate`, `javaNetUrl`): the choice there is the type, not the module. And Java's
own concept words stay Java's where they name the concept rather than the source:
`platformThreading` and `virtualThreading` are platform and virtual threads;
`systemClassloader` and `platformClassloader` are the JDK's names for those loaders.

`native` never appears alone. It has meant Scala Native, the JDK's `WatchService` and
Soundness's own HTTP stack at different times, which is three meanings too many.

### 6. Top-level orphans are named for their library

A given that cannot live in a companion (its subject is owned elsewhere, or is a union
or alias, or the library is a platform component of a platform-neutral type) stays at
the library's top level with a name qualified by what it is for: `tarPathOpenable`,
`terminalStdio`, `collationComparable`. It is never anonymous, since an unnameable
given cannot be imported selectively or disambiguated, and never a bare generic word.

## Sanctioned without a suffix

These pass rule 2 because the choice already names the role, and are listed so that
nobody re-litigates them: `sortingAlgorithms.*`, `alphabets.*`,
`blockCipherModes.{cbc, ctr, cfb, ofb}`, `blockCipherPaddings.{pkcs7, iso10126}`,
`regexBackends.re2`, `internetAccess.{online, offline}`, `endianness.*`,
`htmlDoms.{whatwg, html4Transitional}`, `currencies.Usd…` (ISO codes, used as terms),
`strategies.*`, `calendars.*`, `highlighting.*`, `caseSensitivity.*`,
`dysasymptotics.*`, and `context.explainMissingContext`.

## Checking

`make build` runs `etc/check-given-uniqueness.py`, which reads every choice package in
the libraries and their `soundness_*` export mirrors and fails on a duplicate name or an
unmirrored family. A new orphan given is checked against the whole namespace when it is
added, not when a user meets the clash.
