A clear and consistent coding style makes it easier to read and understand
code. This is particularly true in a complex language like Scala. This document
outlines a consistent strategy for formatting code. It has evolved gradually
over many years, and represents a subjective but informed opinion on code
formatting.

These coding standards relate facets of the source code which have no effect on
the code's behavior, and for which the programmer can make arbitrary choices
about the exact sequence of letters, numbers, symbols and spaces in the source
code without having any effect on the bytecode produced by the compiler.

This document describes writing Scala 3 using its "fewer braces" or "significant
whitespace" style. Indentation is two spaces more than the previous line
Indentation is normally two spaces, but four and eight spaces
are also used in particular circumstances, and arbitrary amounts of indentation
(greater than four spaces) may be used for vertical alignment. Tabs are never
used.

Lines are formatted to 112 characters. Lines longer than 112 characters are
permitted only if there is no way to break them into multiple lines. Much of
this document details how long lines should be split consistently.

### Definitions

Definitions introduce new terms or types, and begin with a keyword: `class`,
`trait`, `type`, `def`, `val`, `var` or `given`, possibly preceded by modifiers
such as `abstract`, `inline`, `private` or `infix`, and followed by an
identifier (except, sometimes, for `given`). Modifiers, definition keywords and
identifiers should always appear on the same line, separated by a single space.

The visibility modifiers, `private` and `protected`, may also be qualified with
the name of an enclosing scope in square brackets, which should appear
immediately afterwards, without any space.

For example,
- `abstract class Date`
- `infix type !!`
- `def run`
- `val state`
- `private[app] var count`
- `given consumer`

Depending on the type of definition


