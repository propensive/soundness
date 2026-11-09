## Environment

### About

A program's surroundings are available as typed [contextual values][contextual value]: its
environment variables, the JVM's system properties, and the working directory it was started
in. Reading a variable gives a value of the type it should hold. That is an `Int` where an
integer is expected, and a path where a path is expected. A variable which is not set raises a
typed error. It never gives a `null` or an empty string.

Each source of ambient input is a [capability], supplied as a [given]. A program's dependence
on its environment is therefore visible in its signatures, and a test can hand it a fixed
environment in place of the real one.

### On the environment

The values which a program inherits when it starts are, in the raw, a bag of untyped strings
reached through global static calls. `System.getenv` returns a `String` or `null`. A property
which should be a number arrives as text. Nothing records that a piece of code depends on a
particular variable being set. The dependence is invisible, and the parsing is left for each
caller to get right.

Here each source is a capability. An `Environment`, a `System` and a `WorkingDirectory` are
contextual values which a method requires when it reads from them, so the dependence is in the
type, as [honest signatures](../philosophy/honest-signatures.md) require. Each read is decoded
to the expected type, and fails loudly when a required value is missing. And since the
capability is a given, substituting a controlled environment, for a test or to sandbox a
component, is a matter of a different import.

Everything comes from the `soundness` package. The real environment and system properties are
supplied by imports, and the last import lets the samples print their results:

```scala
import soundness.*

import environments.javaBaseEnvironment
import strategies.throwUnsafely
import systems.javaBaseSystem
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Environment variables

A variable is read as a member of `Environment`. Its name is written in camel case, and is
translated to the conventional upper-case form with underscores, so `Environment.editor` reads
`EDITOR`. The result has the type which the variable is known to hold, decoded from its text:

```scala fuel
typecheck
##
val home: Text = Environment.home
val columns: Int = Environment.columns
```

A variable which is not set raises an `Environment.Error`. The [strategy] in scope turns that
into an exception, an absent value or a handled failure, as the caller chooses:

```scala fuel
run
errors show
##
Environment.thisVariableIsNotSet[Text]
```

### Overriding for a block

The environment is a capability, so a block can be run against a different one. An
`Environment` is a single function, from the name of a variable to its
[optional](optional-values.md) value. One which is built for a test, or which layers overrides
on the real environment, is a few lines, and a given in a narrower scope takes precedence:

```scala fuel
run
##
locally:
  given environment: Environment = name => if name == t"EDITOR" then t"vim" else Unset
  Out.println(Environment.editor[Text])
```

Substituting an entirely different environment replaces every lookup within its scope.
`environments.emptyEnvironment` is one such, and code which reads the environment can then be
exercised without depending on the machine it runs on. `systems.emptySystem` does the same for
system properties, so a test need not depend on the JVM which happens to run it either.
[Testing](testing.md) covers the rest of what a test needs.

Tests are not the only case. A resident [daemon](daemons.md) serves invocations from many
different shells over its lifetime, each with its own environment, and the JVM's global
environment belongs to whichever shell happened to start the daemon. Taking the environment as
a capability is what lets each invocation be served against the environment it actually came
from.

`variables` sets one variable for one block. The variable is named in camel case, and is set
in the conventional upper-case form, so `gitIndexFile` is `GIT_INDEX_FILE`. It is set on top
of the `Environment` in scope, so a [subprocess](processes.md) which is run within the block
inherits it:

```scala fuel
run
##
variables(gitIndexFile = t"/tmp/index"):
  Out.println(Environment.gitIndexFile[Text])
```

### System properties

The two sources are alike in carrying textual pairs of keys and values into a JVM. They differ
in where they come from, and in how they are named:

```tabular
column  Source
column  Comes from
column  Named in
column  For example
column  When missing
row  environment variables  the shell that started the process  upper case  `PATH`, `XDG_CONFIG_HOME`  `Environment.Error`
row  system properties  `-D` arguments to `java` itself  dotted lower- or camel-case  `default.log.directory`  `Property.Error`
```

Both arrive as text, in a format which only the reading program knows. That is the argument
for [decoding](decoding.md) them into structured types where they are read, and not passing
strings inward.

A system property is read through `System.properties`. Its dotted name is written as a path
of members, which ends in an application. Each property is decoded to its natural type:

```scala fuel
typecheck
##
val user: Text = System.properties.user.name()
val os: Text = System.properties.os.name()
val separator: Char = System.properties.file.separator()
```

A property which is not defined raises a `Property.Error`, which is handled like any other
error.

### The working directory

The directory which a program was launched in is a `WorkingDirectory` capability. It is
requested where it is needed, and is not read from a global. With
`workingDirectories.javaBaseWorkingDirectory` in scope, a method which resolves a relative
[path](paths.md) against the working directory declares that need in its signature.

`workingDirectory` gives the directory itself, and `temporaryDirectory` gives a temporary
one. Each is resolved to the type of path which is asked for.

### Standard directories

The
[XDG base directory specification](https://specifications.freedesktop.org/basedir-spec/latest/)
says where a program's data, configuration, cache and state belong. It honors the user's
environment where that is set, and falls back to the specification's defaults where it is
not. `Xdg` gives each of them, and the search paths for data and configuration:

```scala fuel
typecheck
##
import pathInterfaces.pathOnLinux

val config = Xdg.configHome[Path on Linux]
val cache = Xdg.cacheHome[Path on Linux]
val search = Xdg.dataDirs[Path on Linux]
```

`configHome` is `~/.config`, unless `XDG_CONFIG_HOME` says otherwise. Using these, instead of
a hard-coded `~/.myapp`, makes a program's files land where the user's tools for backup,
synchronization and cleaning expect them.

The conventional layout of a Unix system is typed too. `Home` and `Base` are each a
`BaseLayout`: a tree of nested objects which name the directories under the user's home, and
under the root of the filesystem. Applying one, with the type of path as its type argument,
resolves it against the `Environment` and `System` in scope:

```scala fuel
typecheck
##
val userConfig = Home.Config[Path on Linux]()
val share = Home.Local.Share[Path on Linux]()
val logs = Base.Var.Log[Path on Linux]()
```

These are `~/.config`, `~/.local/share` and `/var/log`. Each layout also knows whether it is
meant to be written to. `readOnly` is `true` for `/usr` and everything beneath it, among
others, so a program can check before it tries:

```scala fuel
run
##
Out.println(Base.Usr.Bin.readOnly.inspect)
Out.println(Base.Etc.readOnly.inspect)
```

The layouts are listed in full, with their `readOnly` values, under
[standard directories](filesystem.md#standard-directories).

### The machine

`Architecture` names the processor which a program is running on. It is parsed from the
platform's own report into a typed value, such as `X86(64)`, `Arm(64)` or
`Ppc(64, littleEndian = true)`. Code which must choose a native library, or a path through the
code, by architecture therefore matches on a value, and not on a string whose spelling varies
between platforms.

`Os` does the same for the operating system. It is decoded from the many spellings which the
property `os.name` takes, by matching the start of it, in any case:

```tabular
column  Case
column  When `os.name` begins
column  For example
row  `Os.Linux`  `linux`  `Linux`
row  `Os.MacOs`  `mac` or `darwin`  `Mac OS X`
row  `Os.Windows`  `windows`  `Windows Server 2022`
row  `Os.FreeBsd`  `freebsd`  `FreeBSD`
row  `Os.OpenBsd`  `openbsd`  `OpenBSD`
row  `Os.NetBsd`  `netbsd`  `NetBSD`
row  `Os.Solaris`  `sunos` or `solaris`  `SunOS`
row  `Os.Aix`  `aix`  `AIX`
row  `Os.Other(name)`  anything else, kept as `name`
```

Both are read from the `System` in scope, so a test can run the same code as though on another
platform, by handing it a fixed `System`. A program which names a release to download, say,
matches on the pair, and not on the raw text of a property:

```scala
val platform: Optional[Text] = (System.os, System.architecture) match
  case (Os.Linux, Architecture.X86(_: 64))   => t"linux-x64"
  case (Os.Linux, Architecture.Arm(_: 64))   => t"linux-arm64"
  case (Os.MacOs, Architecture.Arm(_: 64))   => t"macos-arm64"
  case (Os.Windows, Architecture.X86(_: 64)) => t"windows-x64"
  case _                                     => Unset
```

The third facet of the platform is the virtual machine itself. `Vm` names it, and is where
the events which the VM raises are intercepted: its shutdown, an uncaught exception, a garbage
collection. [Concurrency](concurrency.md) describes `Vm.intercept`.

`termcaps.environmentTermcap` reports what the [terminal](terminal.md) can do. It decides the
depth of color from `COLORTERM` where that is set, and from `tput` otherwise, which is how
styled output degrades correctly on a terminal which cannot show it.

### Related

- [Command-line interfaces](cli.md) reads a program's arguments in the same typed way.
- [Processes](processes.md) starts other programs, which inherit the environment in scope.
- [Filesystem](filesystem.md) reads and writes the files which these directories hold.
- [Classpath](classpath.md) reads the classpath from a system property, and [internationalization](internationalization.md) the locale from the environment.
