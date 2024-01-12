### Standard directory layout

_Imperial_ provides a structure of nested objects inside the `Root` and `Home` objects which mirror the standard directory layout on
modern UNIX systems, as described in the
(XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html). This allows standard
directories to be referenced in code without the risk of typos.

Additionally, certain environment variables may be set which change the standard location, particularly within a user's home
directory. Imperial will take these into account, according to the specification.

### Accessing Directories

Objects have the same names as their corresponding UNIX directory, but by convention begin with a capital letter. Any `.` prefixes in
a directory's name are removed since, of course, `.` is used in place of `/` to access a nested object. So `/var/lib` would be
represented by the object, `Root.Var.Lib` and `$HOME/.local/bin` would be `Home.Local.Bin`.

Conversely, `Home.Config` would normally resolve to `$HOME/.config`, but if the user has the `XDG_CONFIG_HOME` environment
variable set to something else, it would simply resolve to `$XDG_CONFIG_HOME`.

The type of each of these objects is `BaseLayout`, which provides a `BaseLayout#apply()` method for constructing an instance of
a directory type (such as `java.io.File` or `galilei.Directory`) representing the directory. Imperial is designed to be
compatible with different file and directory representations, but it works automatically with
[Galilei](https://github.com/propensive/galilei).

For example, it would be possible to access a `myapp` configuration directory with,
```scala
import galilei.*
val dir: Directory = Home.Config() / t"myapp"
```

### Source of Environment Variables

By default, Imperial will take its environment variables directly from the JVM, since they are globally accessible. But this
is not always the correct source, for example in a long-running server which may receive multiple command-line requests from
different shells over its lifetime, each with a potentially different set of environment variables.

To cater for this scenario, it's possible to provide an alternative `EnvVarProvider` instance. This is a
single-abstract-method trait which takes a `Text` value and returns an `Option[Text]` of the value (returning `None` if the
environment variable is unset).

For example, if the `envvars` is a `Map` of values, a new EnvVarProvider may be specified in-scope with,
```scala
given EnvVarProvider = envvars.get(_)
```



