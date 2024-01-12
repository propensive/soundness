_Harlequin_ provides only a single method, `ScalaSyntax.highlight`, which takes a `Text` value and
returns a `List[Token]`, like so:
```scala
import harlequin.*, gossamer.*

val code = t"def inc(x: Int): Unit =\n  x + 1"
val tokens = ScalaSyntax.highlight(code)
```

The returned value, `tokens`, for this example will be the following `List`:
```scala
List(Code("def", Keyword), Space(1), Code("inc", Term), Code("(", Parens), Code("x", Term),
    Code(":", Symbol), Space(1), Code("Int", Type), Code(")", Parens), Code(":", Symbol), Space(1),
    Code("Unit", Type), Space(1), Code("=", Symbol), Newline, Space(2), Code("x", Ident), Space(1),
    Code("+", Ident), Space(1), Code("1", Number))
```

Here, each token is either, a space of a particular size, for example, `Space(2)` represents two
space characters, a `Newline`, or a fragment of code, consisting of the text with an "accent"; an
indication of the meaning of that code token.

### Accents

The accent will be one of the following possible values:

- `Error`, an erroneous token,
- `Ident`, a reference to a term,
- `Keyword`, a non-modifier keyword,
- `Modifier`, a modifier keyword,
- `Number`, a number literal,
- `Parens`, parenthesis,
- `String`, a string literal (including interpolated strings),
- `Term`, a term definition,
- `Type`, a type definition.

Typically, these would be mapped to different colors during conversion to markup.

It is likely that as Harlequin evolves, the set of `Accent` values will grow.



