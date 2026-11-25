# A backslash is a backslash

A regular expression in a Java string is escaped twice: once for the string, and again for the
pattern. Inside `r"…"` only the rules of regular expressions apply, so the pattern in the
source is the pattern.

```scala fuel
compile
frame
  label Escaped for the string, and for the pattern
  highlight "\\(\\d+\\)"  six backslashes for three
  code
      val year = java.util.regex.Pattern.compile("\\(\\d+\\)")
frame
  label Escaped for the pattern
  highlight r"\(\d+\)"  three for three
  code
      val year = r"\(\d+\)"
##
```

[Escaping](../../soundness/modules/patterns.md#escaping) names the one exception.
