All Ambience terms and types are in the `ambience` package.

```scala
import ambience.*
````

All other entities are imported explicitly.

### Environment Variables

An environment variable, such as `XDG_DATA_DIRS`, can be accessed by applying
it as a `Text` value, to the `Environment` object, like so:
```scala
import gossamer.t
import environments.jvm
import perforate.errorHandlers.throwUnsafely

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
import anticipation.Text
import rudiments.Pid
import spectacular.decodeAs

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
import vacuous.Unset

val vars = Map(t"HOME" -> t"/home/root")
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
import systemProperties.jvm

val home = Properties.user.home()
```
or,
```scala
val dir = Properties.db.user.cache.dir()
```
where the empty parentheses are necessary to signal that the path representing
the property name has been specified, and its value should be retrieved. The
retrieval itself works in much the same way as for environment variables.


