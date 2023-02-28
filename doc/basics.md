Larceny is a compiler plugin, and can be included in a compilation with the
`-Xplugin:larceny.jar` parameter to `scalac`:
```sh
scalac -d bin -Xplugin:larceny.jar -classpath larceny.jar *.scala`
```

The compiler plugin identifies code blocks whose compilation errors should be
suppressed, which are inside a `captureCompileErrors` block (using any
valid Scala block syntax), for example:
```scala
package com.example

import larceny.*

@main def run(): Unit =
  captureCompileErrors("Hello world".substring("5"))

  captureCompileErrors:
    val x = 8
    println(x.missingMethod)
```

Here, the code inside each `captureCompileErrors` block will never compile:
the first, because `substring` takes an `Int` as a parameter, and the second
because `missingMethod` is not a member of `Int`.

But despite this, if the Larceny plugin is enabled, then the code will compile.

And any invalid code that is _not_ within a `captureCompileErrors` block will
still result in the expected compilation errors.

The compilation error from each `captureCompileErrors` block will be
returned (in a `List`) from each block. We could adjust the code to see them,
like so:
```scala
@main def run(): Unit =
  val errors = captureCompileErrors:
    "Hello world".substring("5")

  errors.foreach:
    case CompileError(message, code, offset) =>
      println(s"Found error '$message' in the code '$code' with offset $offset")
```

The three parameters of `CompileError` need some explanation:
- `message` is the human-readable error message text that would be output by
  the compiler
- `code` is the fragment of code which would be marked as problematic (often
  with a wavy red underline)
- `offset` is the number of characters from the start of `code` that is
  indicated as the exact point of the error

Taking the second example above,
```scala
captureCompileErrors:
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

### Implementation

Larceny runs on each source file before typechecking, but after parsing. Any
blocks named `captureCompileErrors` found in the the untyped AST will trigger
a new and independent compilation of the same source file (with the same
classpath, but without the Larceny plugin) from _within_ the main compilation.

Since the `captureCompileErrors` blocks should contain compile errors, this
child compilation is expected to fail, but its compilation errors will be
captured. Each compilation error which is positioned within a
`captureCompileErrors` block will be converted to static code which constructs
a new `CompileError` instance, and inserted into the `captureCompileErrors`
block, in place of entire erroneous contents.

The main compilation is then allowed to continue to typechecking, which will
only see the `CompileError` constructions, not the original code. As long as
there are no compilation errors _outside_ of a `captureCompileErrors` block,
compilation should succeed. When the code is run, each `captureCompileErrors`
block will simply return a list of `CompileError`s.

### Probably

Larceny works well with [Probably](https://github.com/propensive/probably/).

For example, we could write a compile error test with,
```scala
test(t"cannot sort data without an Ordering"):
  captureCompileErrors(data.sorted).head.message
.assert(_.startsWith("No implicit Ordering"))
```

