## Patterns

### About

Soundness matches text against [regular expressions](https://en.wikipedia.org/wiki/Regular_expression)
and [globs](https://en.wikipedia.org/wiki/Glob_(programming)), and it checks the
pattern as the code compiles. A malformed expression is a compile error, and each
capturing group binds to a named, typed variable in the match — so both the
pattern and the values it yields are verified before the program runs.

### On patterns

A regular expression is a small program in its own right, and like any program it
can be wrong. The brackets might not balance, a repetition count might read
`{2,1}`, a capture group referenced by number might not exist. In most languages
these mistakes surface only when the expression is first used at runtime, often
deep in a rarely-taken branch, and a capture is pulled out by an integer index
that no one checks.

Soundness parses the expression with its own scanner while the surrounding code
compiles. A pattern that cannot be parsed never reaches a running program, and the
groups it captures are bound to ordinary Scala variables by name rather than by
number. The type of each binding follows from the pattern: a plain group yields
the matched `Text`, a single character class yields a `Char`, a repeated group
yields a `List`, and an optional group yields an `Optional`. Reading a capture is
therefore as safe as reading any other local value.

Globs — the familiar `*` and `?` wildcards of the command line — are built from
the same machinery, compiling down to regular expressions. Everything comes from
the `soundness` package:

```scala
import soundness.*
```

### Matching

The `r"…"` interpolator writes a regular expression, and in a `match` it behaves as
a pattern. A literal expression matches the text exactly:

```scala
t"hello world" match
  case r"hello world" => true
  case _              => false
// true
```

When a single pattern is expected to match and no fallback makes sense, `absolve`
states that expectation, turning a failure to match into a thrown error rather
than a compiler warning about an inexhaustive match:

```scala
t"hello world".absolve match
  case r"hello world" => 1
// 1
```

### Capturing

A capturing group written with `$name(…)` binds the text it matches to `name`:

```scala
t"hello world" match
  case r"$first(hello) world" => first
  case _                      => t""
// t"hello"
```

Several groups bind several variables, and groups nest:

```scala
t"hello world" match
  case r"$first(hello) $second(world)" => List(first, second)
  case _                               => Nil
// List(t"hello", t"world")

t"hello world" match
  case r"(($first(hello)) world)" => first
  case _                          => t""
// t"hello"
```

A group without a `$name` is matched but not bound, so a pattern can require
structure it does not capture:

```scala
t"hello world" match
  case r"(hello) $second(world)" => second
  case _                         => t""
// t"world"
```

### Characters and quantifiers

A character class binds a single `Char` rather than a `Text`:

```scala
t"hello" match
  case r"h$vowel[aeiou]llo" => vowel
  case _                    => ' '
// 'e'
```

The usual shorthands are available — `\d` for a digit, `\w` for a word character,
`\s` for whitespace, and `.` for any character — and each binds a `Char`:

```scala
t"foo5bar" match
  case r"foo$digit\dbar" => digit
  case _                 => ' '
// '5'
```

A quantifier changes the shape of the binding, and the type changes with it. One
or more (`+`) and zero or more (`*`) bind a `List`; the list is empty when the
pattern matches nothing:

```scala
t"foo123bar" match
  case r"foo$digits\d+bar" => digits
  case _                   => Nil
// List('1', '2', '3')

t"foobar" match
  case r"foo$digits\d*bar" => digits
  case _                   => Nil
// Nil
```

An optional element (`?`) binds an `Optional`, which is `Unset` when the element is
absent:

```scala
t"foo5bar" match
  case r"foo$digit\d?bar" => digit
  case _                  => Unset
// '5'

t"foobar" match
  case r"foo$digit\d?bar" => digit
  case _                  => Unset
// Unset
```

Bounded repetitions follow the same rule, binding a `List`:

```scala
t"foo!?#bar" match
  case r"foo$chars.{2,4}bar" => chars
  case _                     => Nil
// List('!', '?', '#')
```

The binding's type — `Char`, `Text`, `List`, or `Optional` — is read directly from
the pattern, so the way a capture is used and the way it was written can never
drift apart.

### Patterns that always match

A pattern can also bind on the left of a definition, where it stands for an
extraction that the code asserts will succeed. This pulls several captures out of
one expression at once:

```scala
val r"$prefix([a-z0-9._%+-]+)@$domain([a-z0-9.-]+)\.$tld([a-z]{2,6})" =
  t"test@example.com": @unchecked

List(prefix, domain, tld)
// List(t"test", t"example", t"com")
```

### Compiletime checking

A regular expression that cannot be parsed is rejected where it is written, with a
message that points at the offending character. An unbalanced bracket is caught
before the program runs:

```scala
t"" match
  case r"hello(world" =>   // does not compile: the group is never closed
```

So is an interpolated variable that does not stand in front of a group to capture:

```scala
t"" match
  case r"hello${space}world" =>   // does not compile: nothing for `space` to bind
```

The same scanner is available at runtime for patterns built from dynamic text,
where it reports a `RegexError` naming the position and the reason — a bad
repetition such as `{2,1}`, an unclosed group, or a capture inside a repeating
group, which cannot bind a fixed set of variables and so is forbidden.

### Globs

A glob is the wildcard syntax of file paths, and the `g"…"` interpolator matches
one. A `$name` captures a path segment, `*` matches anything within a segment,
`**` matches across segments, and `?` matches a single character:

```scala
t"/home/work/docs" match
  case g"/$home/work/docs" => home
  case _                   => t""
// t"home"

t"/home/work/docs" match
  case g"/$home/*/docs" => home
  case _                => t""
// t"home"

t"/home/work/docs" match
  case g"/$home/**" => home
  case _            => t""
// t"home"
```

Several segments capture at once:

```scala
t"/home/work/docs" match
  case g"/$home/$work/docs" => (home, work)
  case _                    => (t"", t"")
// (t"home", t"work")
```

Because a glob compiles to a regular expression, its translation can be inspected
directly. The wildcards become their regular-expression equivalents, and character
ranges carry across, with `!` meaning negation:

```scala
Glob.parse(t"hello*world").regex        // t"hello[^/\\]*world"
Glob.parse(t"hello[!a-z]world").regex   // t"hello[^a-z]world"
```
