## Environment

### About

A program's surroundings — its environment variables, the JVM's system properties, the
working directory it was started in — are available as typed, contextual values. Reading a
variable gives a value of the type it should hold, an `Int` where an integer is expected and
a path where a path is expected, and a variable that is not set raises a typed error rather
than returning a `null` or an empty string.

Each source of ambient input is a capability supplied as a `given`, so a program's dependence
on its environment is visible in its signatures, and a test can hand it a fixed environment in
place of the real one.

### On the environment

The values a program inherits at startup are, in the raw, a bag of untyped strings reached
through global static calls. `System.getenv` returns a `String` or `null`; a property that
should be a number arrives as text; and nothing records that a piece of code depends on a
particular variable being set. The dependence is invisible, and the parsing is left to each
caller to get right.

Taking the environment as a capability rather than a global is what [honest signatures](../philosophy/honest-signatures.md) require: a method's dependence on its surroundings is visible in its type.

Soundness turns each source into a capability. An `Environment`, a `System`, a
`WorkingDirectory` are contextual values a method requires when it reads from them, so the
dependence is in the type; and each read decodes to the expected type, failing loudly when a
required value is missing. Because the capability is a given, substituting a controlled
environment — for a test, or to sandbox a component — is a matter of a different import.
Everything comes from the `soundness` package, with the real environment and properties
supplied:

```scala
import soundness.*

import environments.javaBaseEnvironment
import strategies.throwUnsafely
import systems.javaBaseSystem
```

### Environment variables

A variable is read as a member of `Environment`, its camel-case name translated to the
conventional upper-snake-case form — `Environment.editor` reads `EDITOR`. The result takes the
type the variable is known to hold, decoded from its text:

```scala
Environment.home      // Text — the value of HOME
Environment.columns   // Int  — COLUMNS, decoded to a number
```

A variable that is not set raises an `Environment.Error`, which the strategy in scope turns into
an exception, an absent value, or a handled failure as the caller chooses.

### System properties

The two sources are alike in carrying textual key/value data into a JVM, and differ in where they
come from and how they are named. Environment variables are inherited from the shell that started
the process and conventionally have upper-case names such as `PATH` or `XDG_CONFIG_HOME`. System
properties are passed as `-D` arguments to `java` itself, and are named in dotted lower- or
camel-case, such as `default.log.directory`. Both arrive as text whose format only the reading
program knows, which is the argument for decoding them into structured types at the point of
reading rather than passing strings inward.

A JVM system property is read through `System.properties`, its dotted name written as a path of
members ending in an application. Each property is decoded to its natural type:

```scala
System.properties.user.name()      // Text
System.properties.os.name()        // Text
System.properties.file.separator() // Char
```

An undefined property raises a `Property.Error`, handled like any other.

### The working directory

The directory a program was launched in is a `WorkingDirectory` capability, requested where it
is needed rather than read from a global. With `workingDirectories.javaBaseWorkingDirectory` in
scope, a method that resolves a relative path against the working directory declares that need
in its signature.

### Overriding for a block

Because the environment is a capability, a block can be run against a different one. An
`Environment` is a single function from a variable's name to its optional value, so one built
for a test — or one layering overrides on the real environment — is a few lines, and a given in
a narrower scope takes precedence:

```scala
locally:
  given Environment = name => if name == t"EDITOR" then t"vim" else Unset
  Environment.editor   // t"vim" within this block
```

Substituting an entirely different environment — `environments.emptyEnvironment`, or one built
for a test — replaces every lookup within its scope, so code that reads the environment can be
exercised without depending on the machine it runs on. `systems.emptySystem` does the same for
system properties, so a test need not depend on the JVM it happens to run under either.

Tests are not the only case. A resident [daemon](daemons.md) serves invocations from many
different shells over its lifetime, each with its own environment, and the JVM's global one
belongs to whichever shell happened to start the daemon. Taking the environment as a capability
is what lets each invocation be served against the environment it actually came from.

### Standard directories

The [XDG base directory specification](https://specifications.freedesktop.org/basedir-spec/latest/)
says where a program's data, configuration, cache and state belong, honoring the user's
environment where it is set and falling back to the specification's defaults where it is not.
`Xdg` gives each of them, and the search paths for data and configuration:

```scala
import pathInterfaces.pathOnLinux

Xdg.configHome[Path on Linux]   // ~/.config, or $XDG_CONFIG_HOME
Xdg.cacheHome[Path on Linux]
Xdg.dataDirs[Path on Linux]     // the search path, in order
```

Using these rather than a hard-coded `~/.myapp` is what makes a program's files land where the
user's backup, sync and cleanup tools expect them.

A temporary directory comes from `temporaryDirectory`, and the directory the program was launched
in from `workingDirectory`, each resolved to the path type asked for.

### The machine

`Architecture` names the processor a program is running on, parsed from the platform's own
reporting into a typed value — `X86(64)`, `Arm(64)`, `Ppc(64, littleEndian = true)` and the rest —
so code that must choose a native library or a code path by architecture matches on a value
rather than on a string whose spelling varies by platform.

`termcaps.environmentTermcap` reports what the terminal can do, deciding color depth from
`COLORTERM` where it is set and from `tput` otherwise, which is how styled output degrades
correctly on a terminal that cannot show it.
