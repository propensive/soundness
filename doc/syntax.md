# Soundness Syntax and Formatting

This document defines the syntactic and whitespace conventions used in the
Soundness libraries. Examples are drawn verbatim from `lib/*/src/core/*.scala`.

## 1. Foundations

### Indentation

Two spaces, no tabs, applied uniformly to every level of nesting and to every
continuation line.

### Indented (colon) syntax over braces

Scala 3's indented (colon) syntax is preferred everywhere. Braces are used only
for short block lambdas, e.g. `{ key => (key, dependencies(key)) }`.

### Line length

Hard limit: 100 columns. Lines that would exceed this must be broken; refer to
the rules for heavy signatures (§4) and for chain continuation (§5.2).

### Operator spacing — precedence-driven

Spacing around operators encodes their algebraic precedence visually:

- `+` and `-` (arithmetic): spaces around them, e.g. `left + right`.
- `*` and `/` (arithmetic): no spaces, e.g. `left*right`, `left/right`.
- `+` for `String` or `Text` concatenation: no spaces. (It is not arithmetic
  and binds tighter than the surrounding context.)
- Set, bit, type-union and type-intersection operators (`&`, `|`, `&~`):
  spaces around them.
- Lambda and arrow operators (`=>`, `->`, `<-`): spaces around them.
- Type-bound operators (`<:`, `>:`): spaces around them.
- Custom infix words (`is`, `of`, `in`, `by`, `to`, `under`, `on`, `raises`):
  spaces around them.

### Brackets, parentheses and commas

- One space after every comma.
- No spaces inside `(…)` or `[…]` on a single line: `(a, b)`, `Map[K, V]`.
- Inside parameter blocks broken across multiple lines, parentheses gain a
  single space inside: `( name: Type, … )`. This is the only context where
  spaces appear inside parentheses (§4).

### Blank lines

A maximum of two consecutive blank lines is permitted anywhere. The exact
number around members is determined by the rules in §6.

## 2. File structure

### License header

Every file begins with the canonical 32-line ASCII-art license header inside a
single `/* … */` block. Lines 1–32 are reserved for it. No exceptions.

### Package declaration

Line 33 is `package <module>` (single segment). No leading blank line, no
trailing blank line on the same line, exactly one blank line between the
package declaration and the first import.

### Imports

Imports are grouped, with one blank line between groups and alphabetical
ordering within each group. The groups appear in this order:

1. `language.*` (and other `import language.…` directives)
2. `java.*` and `javax.*`
3. `scala.*`
4. Soundness library imports

Each group is internally sorted alphabetically. After the final import group,
one blank line precedes the first declaration. Wildcard imports
(`import anticipation.*`) are the norm for library imports.

### File-naming conventions

A module's source files follow these patterns:

- `module.TypeName.scala` — a single class, trait, enum or object plus its
  companion object. The package given block for that type lives in the
  companion.
- `module_core.scala` — top-level extensions, package-level given blocks, and
  nested package blocks for the module.
- `soundness_module_core.scala` — re-exports under the umbrella package
  `soundness`. Contains only `package soundness` and one or more `export`
  statements.
- `module.internal.scala`, `module.protointernal.scala`,
  `module.anteprotointernal.scala` — implementation-detail traits and objects
  layered to satisfy compile-time ordering of givens. The prefixes denote
  successively earlier layers in the resolution order.

## 3. Top-level type declarations

### Single-line vs heavy headers

A class, trait, object or enum header is either _single-line_ (everything up
to and including the body-introducing `:` fits on one line) or _heavy_ (broken
across lines). The choice is forced: if the single-line form would exceed 100
columns, the header must be heavy.

### Heavy header shape

When heavy, the components appear on separate lines:

```scala
class AnnotatedFields[operand <: StaticAnnotation, self, plane, limit]
  ( annotations0: Set[operand], fields0: Map[Text, Set[operand]] )
extends Fields:
```

- The type-parameter list stays on the declaration line.
- The constructor parameter list appears indented 2 spaces, with single spaces
  inside the parentheses.
- `extends …` is flush-left with the class keyword on its own line.
- The body-introducing `:` terminates the `extends` line.

### Companion ordering

When a type and its companion appear in the same file, place the companion
`object` _before_ the type definition. Cross-companion references are
resolved at use-site, so this ordering keeps the more frequently-read API
surface at the top of the file.

## 4. Members

### Single-line, multi-line, and heavy definitions

A definition has three possible shapes; the shape is determined by length, not
preference:

- **Single-line**: signature _and_ body fit on one line, including the `=`.
  ```scala
  def keys: Set[node] = edgeMap.keySet
  ```
- **Multi-line**: signature fits on one line ending with `=`, body wraps to
  the next line(s) indented 2 spaces.
  ```scala
  def map[node2](lambda: node => node2): Dag[node2] =
    Dag(edgeMap.map { (k, v) => (lambda(k), v.map(lambda)) })
  ```
- **Heavy-signature**: signature itself does not fit on one line. Use only
  when the single-line signature would exceed 100 columns; otherwise use
  single-line or multi-line.

### Heavy-signature shape

A heavy signature has, in order:

1. The introducer line: `def name[typeParams]` (and the first parameter list
   if it fits there).
2. Each subsequent parameter list on its own line, indented 2 spaces, with a
   single space inside each parenthesis: `( name: Type, … )`.
3. The return-type colon line, flush-left with the `def` keyword, written as
   `:   ReturnType =`. The `:` is followed by **three spaces** before the
   return type (an exception to the general "one space after `:`" rule, used
   here to make the return type stand out at the same column as a 4-space
   indent level would land).
4. A blank line.
5. The body, indented 2 spaces from the `def`.

Example:

```scala
inline def selectDynamic[variable](key: String)
  ( using environment:      Environment,
          reader:           Variable[key.type, variable],
          environmentError: Tactic[EnvironmentError] )
:   variable =

  environment.variable(reader.defaultName).let(reader.read(_)).or:
    raise(EnvironmentError(reader.defaultName)) yet reader.read(Text(""))
```

### `using` clause alignment

Inside a multi-line `using` clause, parameter names are right-padded so that
their `:` characters align in a single column, and types are left-padded so
that they begin in a single column.

### Anonymous given continuation

An anonymous given whose right-hand side does not fit on one line breaks at
the `=>` operator(s). Each continuation line begins with `=>` followed by
enough spaces to land on the next 2-aligned indent column (the _hard space_
rule, §5.5). When type parameters are split across lines, parameter names are
left-padded so that `<:` aligns:

```scala
given addable: [dom        <: Dom,
                leftTopic  <: Label,
                rightTopic <: Label,
                left       <: Html of leftTopic in dom,
                right      <: Html of rightTopic in dom]
=>  left is Addable by right to (Fragment of leftTopic | rightTopic in dom) =

  (left, right) =>
    Fragment(List(left, right).nodes*).of[leftTopic | rightTopic].in[dom]
```

### Vals, vars, lazy vals, type aliases, opaque types

The same single/multi/heavy distinction applies: keep on one line if it fits,
break the right-hand side onto a continuation line otherwise. Type
intersections beyond what fits on one line break after the `=`:

```scala
type ElectricalConductivity =
  Units[-3, Distance] & Units[-1, Mass] & Units[3, Time] & Units[2, Current]
```

### Extension blocks

`extension` declarations may sit at the top level in `_core.scala` files or
nested inside an object. Methods inside an extension block follow the same
single/multi/heavy rules as ordinary `def`s.

### Annotations

An annotation appears on the line directly above the declaration it annotates,
flush-left with that declaration. No blank line between annotation and
declaration:

```scala
@targetName("removeKey")
infix def - (key: node): Dag[node] = Dag(edgeMap - key)
```

### Symbolic-operator method names

When a method's name is a symbolic operator (`+`, `-`, `*`, `/`, `++`, etc.),
a single space separates the operator from the following parameter list or
type-parameter list:

```scala
infix def + (right: Double): Double = left + right
infix def * [rightMin <: Double, rightMax <: Double](right: rightMin ~ rightMax)
```

Alphabetic method names follow the usual rule (no space before the parameter
list).

## 5. Expressions

### Match expressions

`match` appears on the same line as the scrutinee. Cases are at +2 indent:

```scala
queue match
  case Nil => None

  case (vertex, trace) :: tail =>
    …
```

Within a _run_ of cases whose bodies fit on one line, patterns are right-padded
with spaces so that every `=>` falls in the same column. The alignment column
is determined by the longest pattern in the run:

```scala
case CannotExecuteGit   => m"the `git` command could not be executed"
case CloneFailed        => m"the repository could not be cloned"
case InvalidRepoPath    => m"the repository path was not valid"
…
case CannotSwitchBranch => m"the branch could not be changed"
```

A case whose body wraps onto additional lines ends the run. A blank line
separates the multi-line case from its neighbours; cases that follow a
multi-line case start a fresh alignment run with their own column. Not every
existing block is perfectly aligned; new code should follow the rule.

### Method-call chain continuation

When a chain of method calls wraps to a new line, the continuation begins with
`. method` — a leading dot followed by a single space — at the same indent as
the receiver:

```scala
edgeMap.flatMap:
  case (k, v) => lambda(k).edgeMap.map:
    case (h, w) => (h, (w ++ v.flatMap(lambda(_).keys)))

. reduction
```

This applies to every wrapped chain.

### Function literals

- Inline arrow form: `x => …` or `(x, y) => …` with spaces around `=>`.
- Block form: `{ x => … }` with single spaces inside the braces, used for
  short computations passed as a single argument.
- Multi-line lambdas use indented syntax:
  ```scala
  input =>
    val root = Tag.root(content.reify.map(_.tt).to(Set))
    parse(input.iterator, root).of[content]
  ```

### For-comprehensions and if/else

Use indented syntax. Single-line forms are preferred when they fit:

```scala
for left <- elements; right <- elements
do if element.compare(left, right) then map(left) += right
```

### Hard-space rule

When a short token starts a continuation line — `=>` in a given continuation,
or the return-type `:` of a heavy signature — it is followed by enough spaces
to push the next token to the column it would occupy at the _next_ 2-aligned
indent level after the token itself. In practice this gives:

- `=>  ` (two trailing spaces): `=>` is 2 columns, plus 2 spaces, so the next
  token sits at `=>`'s column + 4.
- `:   ` (three trailing spaces) before a return type: `:` is 1 column, plus
  3 spaces, again giving column + 4.

The `:   Type =` form is the practical one to remember: **three spaces between
the leading `:` and the return type on a heavy-signature return-type line.**

### String interpolators

- `t"…"` for `Text`.
- `m"…"` for `Message`.
- `s"…"` and plain `"…"` only where a raw `String` is genuinely needed.

## 6. Blank-line conventions

Each definition imposes a _padding requirement_: the number of blank lines
required between it and an adjacent sibling definition.

- **Single-line definition**: padding 0.
- **Multi-line definition** (single-line signature, multi-line body): padding 1.
- **Heavy-signature definition**: padding 2.

Between two adjacent siblings, the actual number of blank lines is the
_greater_ of the two padding requirements:

- Two adjacent single-liners with the same keyword: no blank line between them.
- A single-liner next to a multi-line definition: one blank line.
- A multi-line definition next to a heavy-signature definition: two blank
  lines.
- Two heavy-signature definitions: two blank lines.

The "same keyword" relaxation lets runs of `inline given …` or `case …` lines
stack with no separators. A keyword change (e.g. from `given` to `def`)
introduces at least the lower of the two paddings as a separator regardless.

The hard ceiling of two consecutive blank lines is never exceeded.

## 7. Comments

- Line comments use `//` followed by exactly one space and then the comment
  text.
- Block comments (`/* … */`) are reserved for the file-level license header.
- Documentation does not use `/** … */`; prose lives in `doc/` markdown files.
- A comment that pertains to a single line of code may sit at the end of that
  line; otherwise it sits on its own line directly above the code it
  describes, at the same indent.

## 8. Summary checklist

- 2-space indent, no tabs, 100-column hard limit.
- Indented (colon) syntax over braces.
- License header (32 lines) → `package` → blank → grouped imports → blank →
  code.
- Operator spacing: `+`/`-` spaced; `*`/`/` and concatenation `+` unspaced;
  `&`, `|`, `&~`, `=>`, `->`, `<-`, `<:`, `>:` and infix words spaced.
- Single-line / multi-line / heavy-signature definitions; choice forced by
  the 100-column limit.
- Heavy signature: parameter blocks indented 2 with internal spaces; return
  type `:   Type =` flush-left; blank line before body.
- Match cases: `=>` aligned within a single-line run; runs broken by
  multi-line cases.
- Symbolic-operator method names take a space before the parameter list.
- Chain continuation: `. method` at receiver indent.
- Blank-line padding: 0 / 1 / 2 around single-line / multi-line /
  heavy-signature definitions; greater-of rule between unequal neighbours;
  never more than two consecutive blanks.
