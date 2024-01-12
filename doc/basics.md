### Character Display Width

Hieroglyph provides an extension method, `displayWidth`, on `Char` which will
return the amount of space the glyph for that character will take up, when
rendered in a mono-spaced font.

Unsurprisingly, this will usually be `1`, but many characters in alphabets that
are not based on the Latin Alphabet will need two normal character widths of
space when rendered.

For example, compare,
```scala
'x'.displayWidth   // returns 1
'好'.displayWidth  // returns 2
```

However, calculating the width of a character (and, in particular a string of
characters) will be much slower if every character must be checked individually,
and totalled, when the `length` field of a string can provide the same value in
constant (and fast) time, for strings which are known not to contain any "wide"
characters.

[Gossamer](https://github.com/propensive/gossamer/) provides a corresponding
`displayWidth` extension method on all text-like types, which calculates the
display width of the entire string by summing its character widths, or, with
`textWidthCalculation.uniform` in scope, simply returns the `length` value.

Therefore, methods which need to perform text width calculations can use either
a `uniform` mode or an `eastAsianScripts` mode, depending on the contextual
value imported from the `textWidthCalculation` package.



