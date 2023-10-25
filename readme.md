[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/ambience/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/ambience/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Ambience

__Safely access environment variables and system properties__

There are two common methods of passing textual key/value data into a JVM when
it starts: environment variables and system properties. Environment variables
are taken from the shell environment within which the JVM is started, and
usually have uppercase names like `PATH` or `XDG_CONFIG_DIR`, while system
properties are specified as parameters to the `java` command, and have
lowercase or camel-case, period-separated names like `default.log.directory`.
The format of the values of each is dependent on the variable or property, but
a program running on the JVM will have expectations about the format of the
variables and properties it accesses, and it is desirable to parse them into
structured types as early as possible. This is the role of Ambience.

## Features

- typesafe access of environment variables and system properties
- types are inferred for known variables and properties, avoiding strings wherever possible
- concise syntax with idiomatic renaming of environment variables
- complete flexibility to use substitute environments


## Availability

Ambience has not yet been published as a binary.

## Getting Started

### Environment Variables

An environment variable, such as `XDG_DATA_DIRS`, can be accessed by applying
it as a `Text` value, to the `Environment` object, like so:
```scala
import ambience.*, gossamer.*
import environments.jvm
val xdgDataDirs = Environment(t"XDG_DATA_DIRS")
```

Note that we import `environments.jvm` because we want to use the environment
from the running JVM. It's possible to use alternative environments, for
example when testing, but normally we want `environments.jvm`.

Environment variables typically use a naming scheme that is incongruous with
other identifiers in Scala. Environment variable names are typically in
uppercase, with multiple words separated by underscores. So an identifier which
would be written `moduleDataHome` in Scala would be written `MODULE_DATA_HOME`
as an environment variable. Ambience can perform this translation
automatically, just by accessing the variable name as a dynamic member of the
`Environment` object, for example,
```scala
val dirs = Environment.moduleDataHome
```
will access the environment variable `MODULE_DATA_HOME`.

If the variable is not defined in the environment, an `EnvironmentError` will
be raised.

It might be reasonably presumed that in the examples above, string values (as
`Text`s) would be returned, and in general this is true, unless the variable is
_known_ in which case, the variable will be parsed into a more appropriate
representation.

For a variable to be _known_, a contextual `EnvironmentVariable` instance,
parameterized on the singleton type of the environment variable's name (in its
camel-case variant) and a result type, must be in scope. For example,
`Environment.columns` (referring to the `COLUMNS` environment variable) will
return an `Int` thanks to the presence of an
`EnvironmentVariable["columns", Int]` typeclass instance, which is provided by
Ambience since `COLUMNS` is a standard POSIX environment variable name.

However, the `moduleDataHome` example above will default to returning a `Text`
value, since no `EnvironmentVariable` instance for the `"moduleDataHome"`
singleton literal type is provided by Ambience.

We can, of course, provide one. The `EnvironmentVariable` trait defines two methods:
- `read`, which takes a `Text` value and returns a parsed value, and
- `name`, which allows custom renamings from "Scala style" names to a
  particular environment variable, with a default implementation that can be
  overridden

Here is an example implementation for a process ID:
```scala
import spectacular.*

given EnvironmentVariable["child", Pid] with
  def read(value: Text): Pid = Pid(value.decodeAs[Int])
  override def name: Text = t"CHILD_PID"
```

Now, accessing `Environment.child` will read the `CHILD_PID` environment
variable, parse it as an integer and construct a new `Pid` instance. We could
also omit the override of `name` and just access the value as
`Environment.childPid`.

Note that `value.decodeAs[Int]`, which is provided by
[Spectacular](https://github.com/propensive/spectacular/), and uses a
`Decoder[Int]` instance may, given the definition of `Decoder[Int]`, fail with
a `NumberError` if the `CHILD_PID` variable contains a value which isn't an
integer. The capability to raise this error will be aggregated into the
`Environment.child` call, ensuring that it will be handled.

#### Custom `Environment`s

Sometimes it is useful to specify a different source of environment variables
than the JVM. This may be because we need to test a method which has been
written to get a value from the environment, but we don't want to introduce the
nondeterminism that arises when running the tests on different machines with
different environments.

Another example presents itself if we want a method call (which takes an
`Environment`) to see different values for certain environment variables, for
example, to ensure that the method accesses an SSH agent with a particular PID,
rather than the SSH agent specified in the `SSH_AGENT_PID` environment
variable, captured when the JVM started.

The `Environment` typeclass defines just a single `apply` method for accessing
environment variables, taking a `Text` value of the environment variable name,
and returning a `Maybe[Text]`, with `Unset` indicating that the environment
variable is not specified. It is distinct from the empty string.

For example, given a `Map[Text, Text]` of environment variables, `vars`, we
could create a new `Environment` with the following `given` definition:
```scala
given Environment = vars.getOrElse(_, Unset)
```

#### Summary

Together, these facilities provide access to environment variables which is
idiomatic (with Scala-style identifiers), typesafe (using checked errors if
parsing fails), concise (types can be inferred) and flexible (allowing
substitution of entire environments).

### System Properties

Support for system properties is provided in much the same way as for
environment variables:
- access is provided through the `Properties` object, by `Text` value or
  dynamic member access
- `systemProperties.jvm` provides the standard system properties from the JVM
- alternative instances of `SystemProperties` can be defined to provide
  substitute values
- `SystemProperty` provides the same functionality as `EnvironmentVariable`
- a `SystemPropertyError` will be raised instead of an `EnvironmentError`

There is no need to rename system properties, since they already follow the
familiar Scala identifier style. Access through the `Properties` object is
slightly different from `Environment`, though: since property names use a
"dotted" format, they can be accessed as dynamic members of the `Properties`
object, for example,
```scala
val home = Properties.user.home()
```
or,
```scala
val dir = Properties.db.user.cache.dir()
```
where the empty parentheses are necessary to signal that the path representing
the property name has been specified, and its value should be retrieved. The
retrieval itself works in much the same way as for environment variables.




## Status

Ambience is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Ambience is designed to be _small_. Its entire source code currently consists
of 226 lines of code.

## Building

Ambience can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Ambience are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/ambience/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Ambience easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Ambience was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

An _ambience_ is a sense derived from the surrounding environment, and _Ambience_ provides sensible access to the "surrounding environment" of a Scala program.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo depicts the upper atmosphere of an imagined planet, alluding to the synonymous meaning of "ambience".

## License

Ambience is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
