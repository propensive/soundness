# Safety by Construction

Soundness makes invalid values impossible to construct in the first place, checking
them as the code compiles. A guiding conviction is that any static analysis a
programmer can do, the compiler can do better — and any analysis the compiler can do,
it should. A value that exists is therefore known to be well-formed, and the code that
receives it need not check again.

Interpolators express this most clearly. Each little language embedded in a program —
URLs, paths, regular expressions, dates, media types, JSON — has a grammar, and its
literals are parsed against that grammar at compiletime, by the same parser that will
handle runtime input. A malformed literal is a compile error pointing at the offending
character:

```scala
url"https://example.com/api"    // checked against the URL grammar
r"[a-z]+(suffix"                // does not compile: unclosed group
media"application/jsom"         // does not compile: did you mean application/json?
v"1.2"                          // does not compile: not a semantic version
```

The same principle reaches beyond literals. A date that cannot exist — the 31st of a
30-day month — is rejected where it is written; a `Money in "EUR"` cannot be added to a
`Money in "GBP"`; a database relation the schema does not declare fails to compile; and
an HTML `<br>` given children is a compile error because the specification says so.
In each case the check is not a runtime validation moved earlier, but a construction
that only admits valid values — so the invalid state has no representation at all, and
downstream code inherits the guarantee for free.
