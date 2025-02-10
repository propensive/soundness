### About ANSI Codes

[ANSI escape codes](https://en.wikipedia.org/wiki/ANSI_escape_code) provide a variety of features in
terminal emulators for performing operations such as positioning the cursor, changing the appearance
of text with styles like bold, italic and strike-through, as well as foreground and background
colors.

### Creating ANSI strings

To create an ANSI string, we use the `e""` interpolator. This works like an ordinary string
interpolator, allowing substitutions of stringlike values.

But substitutions may also be `Escape` instances, which do not insert any visible characters, but
may change the appearance or style of subsequent characters. Many `Escape` instances are defined in
the `escapes` object, for example, `Bold`, `Underline` and `BrightRedFg`.

These escapes may be used in an ANSI string, like so:
```scala
import escapes.*
val txt = e"This text is ${Bold}bold, ${Underline}underlined and ${BrightRedFg}bright red."
```

This introduces the bold and underline styles and the bright red color to the string, but once
introduced, they continue indefinitely.

Thankfully, Escapade provides a convenient way to terminate styles introduced by ANSI escapes. If
the character immediately following the escape is a recognized opening bracket (`(`, `[`, `{`, `«`,
`<`), then the style will continue until a corresponding closing brace is found in the string, i.e.
`)`, `]`, `}`, `»` or `>`.

For example,
```scala
e"This text is $Bold[bold], $Underline{underlined} and $BrightRedFg<bright red>."
```
will apply each style only to the words inside the brackets.

Plenty of choice is given over which type of brackets to use, so that a choice can (hopefully) be
made which does not conflict with the real content of the string. Regions may be nested arbitrarily
deep, and different bracketing styles may be used, but nested regions form a stack which must be
terminated in order. So any closing bracket other than the type corresponding to the most recent
opening bracket will not be given special treatment.

For example,
```scala
e"This text is $Bold[bold and $Italic{italic] text.}"
```
might be intending to display the final word, `text`, in italic but not bold. But the mismatched
brackets would treat `italic] text.` as literal text, rendered in italic. And, in fact, the ANSI
string would not compile due to the unclosed `[` bracket.

### Combining colors

While styles such as _bold_, _italic_, _underline_ and _reverse_ may be combined independently in a
string, the situation is more complex with colors, as a new color simply replaces an old one, and
it is not normally possible to restore the previous color; only to "reset" the color.

Indeed, this is what happens using the standard ANSI escapes provided in the `escapes` object.

But Escapade also provides stack-based tracking for colored text, so that regions may be nested, and
the underlying color may be restored. This uses colors from
[Iridescence](https://github.com/propensive/iridescence/) which may be substituted straight into an
ANSI string, like so:

```scala
import iridescence.*
import colors.*
e"$Gold[gold, $Indigo[indigo, $HotPink[hot pink], indigo] $White[and] gold]"
```

### Manipulating colors

Each substitution into an `e""` string interpolator may apply a change to the existing style,
represented by and tracked throughout the string as an instance of the case class, `Style`.
Typically, these changes will specify the new state of properties such as _bold_, _italic_ or the
background color.

But the changes may also be a transformation of the existing style information. For example, the
bold state could be flipped depending on what it was before, or the foreground color could be
mixed with black to give a "faded" or "darkened" effect to the text, without changing its hue.

Any such transformation requires an object to be used as a substitution to an interpolated string
to introduce it, plus a corresponding contextual `Stylize` instance, for example:
```scala
case class Fade(amount: Double)

given Stylize[Fade] = fade =>
  Stylize { style =>
    style.copy(fg = style.fg.hsv.shade(fade.amount).srgb)
  }
```



