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

Soundness turns each source into a capability. An `Environment`, a `System`, a
`WorkingDirectory` are contextual values a method requires when it reads from them, so the
dependence is in the type; and each read decodes to the expected type, failing loudly when a
required value is missing. Because the capability is a given, substituting a controlled
environment — for a test, or to sandbox a component — is a matter of a different import.
Everything comes from the `soundness` package, with the real environment and properties
supplied:

```scala
import soundness.*
import environments.javaEnvironment
import systems.javaSystem
import strategies.throwUnsafely
```

### Environment variables

A variable is read as a member of `Environment`, its camel-case name translated to the
conventional upper-snake-case form — `Environment.editor` reads `EDITOR`. The result takes the
type the variable is known to hold, decoded from its text:

```scala
Environment.editor    // Text — the value of EDITOR
Environment.columns   // Int  — COLUMNS, decoded to a number
```

A variable that is not set raises an `EnvironmentError`, which the strategy in scope turns into
an exception, an absent value, or a handled failure as the caller chooses.

### System properties

A JVM system property is read through `System.properties`, its dotted name written as a path of
members ending in an application. Each property is decoded to its natural type:

```scala
System.properties.user.name()      // Text
System.properties.os.name()        // Text
System.properties.file.separator() // Char
```

An undefined property raises a `PropertyError`, handled like any other.

### The working directory

The directory a program was launched in is a `WorkingDirectory` capability, requested where it
is needed rather than read from a global. With `workingDirectories.javaWorkingDirectory` in
scope, a method that resolves a relative path against the working directory declares that need
in its signature.

### Overriding for a block

Because the environment is a capability, a block can be run against a modified one. `variables`
supplies overrides for the code it encloses, leaving the surrounding environment untouched:

```scala
variables(EDITOR = t"vim"):
  Environment.editor   // t"vim" within this block
```

Substituting an entirely different environment — `environments.emptyEnvironment`, or one built
for a test — replaces every lookup within its scope, so code that reads the environment can be
exercised without depending on the machine it runs on.
