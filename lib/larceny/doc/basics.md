Larceny is a compiler plugin, and can be included in a compilation with the
`-Xplugin:larceny.jar` parameter to `scalac`:
```sh
scalac -d bin -Xplugin:larceny.jar -classpath larceny.jar *.scala
```

The compiler plugin identifies code blocks whose compilation errors should be
suppressed, which are inside a `demilitarize` block (using any
valid Scala syntax), for example:
```scala
package com.example

import larceny.*

@main def run(): Unit =
  demilitarize("Hello world".substring("5"))

  demilitarize:
    val x = 8
    println(x.missingMethod)
```

Here, the code inside each `demilitarize` block will never compile:
the first, because `substring` takes an `Int` as a parameter, and the second
because `missingMethod` is not a member of `Int`.

But despite this, if the Larceny plugin is enabled, then the code will compile.

Any invalid code that is _not_ within a `demilitarize` block will
still result in the expected compilation errors.

The compilation error from each `demilitarize` block will be
returned (in a `List`) from each block. We could adjust the code to see them,
like so:
```scala
@main def run(): Unit =
  val errors = demilitarize:
    "Hello world".substring("5")

  errors.foreach:
    case CompileError(ordinal, message, code, position, offset) =>
      println(s"[$id] Found error '$message' in the code '$code' with offset $offset")
```

The parameters of `CompileError` need some explanation:
- `ordinal` is the ordinal identifier representing the type of error; the Scala
  compiler defines about 200 such error types (though some occur more
  frequently than others)
- `message` is the human-readable error message text that would be output by
  the compiler
- `code` is the fragment of code which would be marked as problematic (in an
  IDE, this would usually be done with a wavy red underline)
- `position` is the location of the code from the start of the source file
- `offset` is the number of characters from the start of `code` that is
  marked as the exact point of the error

Taking the second example above,
```scala
demilitarize:
  val x = 8
  println(x.missingMethod)
```
the `message` would be:
```
value missingMethod is not a member of Int
```
while the `code` value would be `x.missingMethod` (note that the surrounding
`println` is not considered erroneous), and the `offset` would be `2`. The
value `2` is because the erroneous code begins `x.`, but the point of the error
is considered to be the `m` of `missingMethod`, which is character `2`.

The error IDs are defined in the Scala compiler and correspond to an
enumeration of values. For convenience, these values have been copied into the
`CompileErrorId` enumeration, and can be accessed by the `id` method of
`CompileError`.

`CompileErrorId` is also an extractor on `CompileError`, so it's possible to
write:
```scala
demilitarize(summon[Ordering[Exception]]) match
  case ErrorId(ErrorId.MissingImplicitArgumentID) => "expected"
  case _                                          => "unexpected"
```

### Implementation

Here are the details of how Larceny works. It should not be necessary to
understand its implementation for normal usage, but as experimental software,
it may behave unexpectedly, and this explanation may help to diagnose
misbehavior.

Larceny runs on each source file before typechecking, but after parsing. Any
blocks named `demilitarize` found in the the untyped AST will trigger
a new and independent compilation of the same source file (with the same
classpath, but without the Larceny plugin) from _within_ the main compilation.

Since the `demilitarize` blocks should contain compile errors, this
child compilation is expected to fail, but its compilation errors will be
captured. Each compilation error which is positioned within a
`demilitarize` block will be converted to static code which constructs
a new `CompileError` instance, and inserts it into the `demilitarize`
block, in place of entire erroneous contents.

If there are multiple `demilitarize` blocks in the same source file,
some errors which occur in earlier phases of compilation may suppress later
phases from running, and the errors from those later phases will not be
captured during the first compilation. Larceny will rerun the compiler as
many times as necessary to capture errors from later phases, each time
removing more code which would have precluded these later phases.

The main compilation is then allowed to continue to typechecking, which will
only see the `CompileError` constructions, not the original code. As long as
there are no compilation errors _outside_ of a `demilitarize` block,
compilation should succeed. When the code is run, each `demilitarize`
block will simply return a list of `CompileError`s.

### Testing Frameworks

Larceny should work with any Scala unit testing framework or library. For
example, with [Probably](https://github.com/propensive/probably/), we could
write a compile error test with:
```scala
test(m"cannot sort data without an Ordering"):
  demilitarize(data.sorted).head.message
.assert(_.startsWith("No implicit Ordering"))
```
