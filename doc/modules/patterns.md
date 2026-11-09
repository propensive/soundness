## Patterns

### About

Text is matched against
[regular expressions](https://en.wikipedia.org/wiki/Regular_expression) and
[globs](https://en.wikipedia.org/wiki/Glob_(programming)), and each pattern is checked as the
code compiles. A malformed expression is a compile error. Each capturing group binds a named,
typed variable in the match.

Both the pattern and the values it yields are therefore verified before the program runs.

### On patterns

A regular expression is a small program, and like any program it can be wrong. The brackets
might not balance. A repetition count might read `{2,1}`. A capture group, referred to by
number, might not exist. In most languages these mistakes appear only when the expression is
first used at [runtime], often deep in a branch which is rarely taken. And a capture is fetched
by an integer index which nothing checks.

Here the expression is parsed while the surrounding code compiles. A pattern which cannot be
parsed never reaches a running program. The groups it captures are bound to ordinary variables,
by name and not by number.

The type of each binding follows from the pattern. A plain group gives the matched `Text`. A
single character class gives a `Char`. A repeated element gives a `List`, and an optional
element gives an [`Optional`](optional-values.md). Reading a capture is as safe as reading any
other local value. This is
[safety by construction](../philosophy/safety-by-construction.md) for regular expressions.

Globs are the `*` and `?` wildcards of the command line. They are built from the same machinery,
and compile to regular expressions.

Everything comes from the `soundness` package, and the second import lets the samples print
their results:

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Matching

The `r"…"` [interpolator] writes a regular expression. In a `match`, it is a pattern. A literal
expression matches the whole text exactly:

```scala fuel
run
##
t"hello world" match
  case r"hello world" => Out.println(t"matched")
  case _              => Out.println(t"no match")
```

The pattern must match all of the text, not just a part of it:

```scala fuel
run
##
t"hello world!" match
  case r"hello world" => Out.println(t"matched")
  case _              => Out.println(t"no match")
```

Sometimes one pattern is expected to match, and no fallback makes sense. `absolve` states that
expectation. The compiler then gives no warning about an inexhaustive match, and a failure to
match is a thrown error:

```scala fuel
run
##
t"hello world".absolve match
  case r"hello world" => Out.println(t"matched")
```

### Capturing

A capturing group written as `$name(…)` binds the text it matches to `name`. Several groups bind
several variables:

```scala fuel
frame
  label A literal pattern, matched or not
  code
      t"hello world" match
        case r"hello world" => true
        case _              => false
frame
  label One group, bound to `first`
  code
      t"hello world" match
        case r"$first(hello) world" => first
        case _                      => t""
frame
  label Two groups, bound to `first` and `second`
  code
      t"hello world" match
        case r"$first(hello) $second(world)" => List(first, second)
        case _                               => Nil
##
```

Each bound name is a `Text`:

```scala fuel
run
##
t"hello world" match
  case r"$first(hello) $second(world)" => Out.println(t"$second, $first")
  case _                               => Out.println(t"no match")
```

Groups nest:

```scala fuel
run
##
t"hello world" match
  case r"(($first(hello)) world)" => Out.println(first)
  case _                          => Out.println(t"no match")
```

A group without a `$name` is matched but not bound. A pattern can therefore require structure
which it does not capture:

```scala fuel
run
##
t"hello world" match
  case r"(hello) $second(world)" => Out.println(second)
  case _                         => Out.println(t"no match")
```

### Characters and quantifiers

A character class binds a single `Char`, not a `Text`:

```scala fuel
run
##
t"hello" match
  case r"h$vowel[aeiou]llo" => Out.println(vowel.inspect)
  case _                    => Out.println(t"no match")
```

The usual shorthands are available: `\d` for a digit, `\w` for a word character, `\s` for
whitespace and `.` for any character. Each binds a `Char`:

```scala fuel
run
##
t"foo5bar" match
  case r"foo$digit\dbar" => Out.println(digit.inspect)
  case _                 => Out.println(t"no match")
```

A quantifier changes the shape of the binding, and its type changes to suit. One or more (`+`)
and zero or more (`*`) bind a `List`:

```scala fuel
run
##
t"foo123bar" match
  case r"foo$digits\d+bar" => Out.println(digits.inspect)
  case _                   => Out.println(t"no match")
```

The list is empty when the element matches nothing:

```scala fuel
run
##
t"foobar" match
  case r"foo$digits\d*bar" => Out.println(digits.inspect)
  case _                   => Out.println(t"no match")
```

An optional element (`?`) binds an `Optional`, which is `Unset` when the element is absent:

```scala
def digit(text: Text): Optional[Char] = text match
  case r"foo$digit\d?bar" => digit
  case _                  => Unset
```

```scala fuel
run
##
Out.println(digit(t"foo5bar").inspect)
Out.println(digit(t"foobar").inspect)
```

A bounded repetition follows the same rule, and binds a `List`:

```scala fuel
run
##
t"foo!?#bar" match
  case r"foo$chars.{2,4}bar" => Out.println(chars.inspect)
  case _                     => Out.println(t"no match")
```

The type of a binding is read directly from the pattern. The way a capture is written and the
way it is used can never drift apart:

```tabular
column  A capture of
column  Such as
column  Binds
row  a group  `$first(hello)`  `Text`
row  a character class, or `\d`, `\w`, `\s` or `.`  `$vowel[aeiou]`  `Char`
row  an element repeated with `+` or `*`  `$digits\d+`  `List`, empty when nothing matches
row  an element repeated a bounded number of times  `$chars.{2,4}`  `List`
row  an optional element, with `?`  `$digit\d?`  `Optional`, `Unset` when absent
```

### Patterns that always match

A pattern can also bind on the left of a definition. There it stands for an extraction which the
code asserts will succeed. This takes several captures out of one expression at once:

```scala
val r"$prefix([a-z0-9._%+-]+)@$domain([a-z0-9.-]+)\.$tld([a-z]{2,6})" =
  t"test@example.com": @unchecked
```

```scala fuel
run
##
Out.println(List(prefix, domain, tld).inspect)
```

### Escaping

Between source code and a compiled expression, escaping normally happens twice: once when the
source is read as a string, and again when the string is read as a pattern. That is why matching
a single backslash in Java means writing `Pattern.compile("\\\\")`. The expression needs two
backslashes, and the string literal doubles each of them.

Only the second level applies here. Inside `r"…"` or `r"""…"""`, a backslash is a backslash, so
only the rules of regular expressions need thought. An opening parenthesis is matched by
`r"\("`, and nothing more. The one exception is `$`, which introduces a substitution, and is
written `$$` to mean itself.

### Expressions as values

Outside a pattern, the same interpolator is an ordinary expression, and gives a `Regex`:

```scala
val ipAddress = r"([0-9]+\.){3}[0-9]+"
```

A `Regex` value can be used as an extractor in a pattern. It matches as a whole, and cannot bind
its capturing groups. Those are available only where the pattern is written literally, since
only there can the compiler see them. The value carries its source as `pattern`, a `Text`, along
with the position and nature of each of its groups.

A `Regex` is used wherever a regular expression is called for, and a `Text` is never used for
one. The signature of a method therefore says whether its argument is read as a pattern or taken
literally. APIs typed with strings leave that distinction to their documentation.
Substitution in [text](text.md) is overloaded on exactly this basis:

```scala
val spaced = t"too   many    spaces"
```

```scala fuel
run
##
Out.println(spaced.sub(r"\s+", t" "))
Out.println(spaced.sub(t"\\s+", t" "))
```

The first call collapses each run of whitespace. The second looks for three literal characters,
finds none, and changes nothing.

A glob compiles to a regular expression, so a `g"…"` is a `Regex` too, and can be used anywhere
that one can. Two patterns which compile to the same expression are equal:

```scala fuel
run
##
Out.println((g".local/*" == r"\.local/[^/\\]*").inspect)
Out.println((r"[xy]" == r"[yx]").inspect)
```

Equality compares the expressions, not what they match. `r"[xy]"` and `r"[yx]"` match exactly
the same inputs, and are still unequal.

### Compiletime checking

A regular expression which cannot be parsed is rejected where it is written. An unbalanced
bracket is caught at [compiletime]:

```scala fuel
compile
errors show
##
t"" match { case r"hello(world" => () }
```

So is an interpolated variable which does not stand in front of a group to capture:

```scala
val space = t" "
```

```scala fuel
compile
errors show
##
t"" match { case r"hello${space}world" => () }
```

The same parser is available at runtime, for a pattern built from dynamic text. It raises a
`Regex.Error` which names the position and the reason:

```scala fuel
run
errors show
##
import strategies.throwUnsafely
Regex(t"hello(world")
```

These are the reasons. A capture inside a repeating group could not bind a fixed set of
variables, and so is forbidden:

```tabular
column  Reason
column  The pattern is rejected because
row  `UnclosedGroup`  a capturing group was not closed
row  `NotInGroup`  a closing parenthesis has no opening one
row  `ExpectedGroup`  an extractor is not followed immediately by a capturing group
row  `Uncapturable`  a capturing group is inside a repeating group
row  `BadRepetition`  a repetition's maximum is less than its minimum, as in `{2,1}`
row  `ZeroMaximum`  a repetition's maximum is zero
row  `IncompleteRepetition`  a repetition range is not closed
row  `UnexpectedChar`  a repetition range contains an unexpected character
row  `UnclosedEscape`  nothing follows the escape character
row  `EmptyCharClass`  a character class is empty
row  `InvalidPattern`  the pattern is otherwise invalid
```

### Choosing an engine

The default engine is the JVM's own. Its backtracking can take exponential time on a
pathological pattern and input. This is
[catastrophic backtracking](https://en.wikipedia.org/wiki/ReDoS), which turns a regular
expression applied to untrusted input into a denial of service. `re2` is an automaton engine
written in Scala, which avoids it. The price is the constructs which an automaton cannot
express:

```tabular
column  Engine
column  Given
column  Time
column  Constructs
row  the JVM's  `regexBackends.javaBaseRegex`, the default  exponential, at worst  every construct of its syntax
row  re2  `regexBackends.re2`  linear in the length of the input  all but backreferences and lookaround
```

The engine is a [given], so a program chooses it with an import, and the patterns are unchanged:

```scala fuel
run
##
locally:
  import regexBackends.re2

  t"hello world" match
    case r"hello $name(.*)" => Out.println(name)
    case _                  => Out.println(t"no match")
```

A literal pattern which uses a construct the engine does not support is rejected as the code
compiles, and not at the first match.

### Globs

A glob is the wildcard syntax of file [paths](paths.md), and the `g"…"` interpolator matches
one:

```tabular
column  Glob
column  Matches
column  As a regular expression
row  `$name`  a path segment, captured as `name`
row  `*`  anything within a segment  `[^/\\]*`
row  `**`  anything, across segments  `.*`
row  `?`  a single character within a segment  `[^/\\]`
row  `[a-z]`, `[abc]`  one character of a range or a set  `[a-z]`, `[abc]`
row  `[!a-z]`, `[!abc]`  one character outside a range or a set  `[^a-z]`, `[^abc]`
```

A `$name` captures one segment of a path:

```scala fuel
run
##
t"/home/work/docs" match
  case g"/$home/work/docs" => Out.println(home)
  case _                   => Out.println(t"no match")
```

`*` stands for one segment, and `**` for any number:

```scala fuel
run
##
t"/home/work/docs" match
  case g"/$home/*/docs" => Out.println(home)
  case _                => Out.println(t"no match")
```

```scala fuel
run
##
t"/home/work/docs" match
  case g"/$home/**" => Out.println(home)
  case _            => Out.println(t"no match")
```

Several segments are captured at once:

```scala fuel
run
##
t"/home/work/docs" match
  case g"/$home/$work/docs" => Out.println(t"$home and $work")
  case _                    => Out.println(t"no match")
```

The translation of a glob to a regular expression can be seen directly. The wildcards become
their equivalents, and character ranges carry across, with `!` meaning negation:

```scala fuel
run
##
Out.println(Glob.parse(t"hello*world").regex)
Out.println(Glob.parse(t"hello[!a-z]world").regex)
```

### Related

- [Text](text.md) describes `sub` and the other operations which take a `Regex`.
- [Interpolation](interpolation.md) explains how an interpolator such as `r"…"` checks its
  content at compiletime.
- [Paths](paths.md) and the [filesystem](filesystem.md) describe the paths which globs match.
- [Optional values](optional-values.md) describes the `Optional` which an optional element
  binds.
- [HTTP server](http-server.md) matches the paths of requests.
- [Decoding](decoding.md) and [TEL](tel.md) match text against patterns while reading it.
