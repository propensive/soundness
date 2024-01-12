### Defining a logger

Libraries which use Eucalyptus for logging will need a contextual `Log` instance before they can
be used. This may be as simple as defining a `given Log` instance in the package where the
libraries are used, for example,
```scala
given Log = logging.silent
```
or,
```scala
given Log = logging.stdout
```
or,
```scala
given Log = logging.syslog
```

A more versatile `Log` can route different log messages to different targets, for example,
```scala
given Log = Log.route:
  case Level.Fail() => Err
  case Level.Warn() => Out
  case _            => Syslog(t"app")
```
would log all `FAIL` level messages to standard error, all `WARN` level messages to standard output, and
everything else to the system log (with the tag `app`).

### Library Code

#### Contextual `Log` instances

Any method which requires logging should request a contextual `Log` parameter. This
is as simple as adding the parameter block, `(using Log)` to the method. Any methods
which call such a method will also need to include a `using Log` parameter.

#### Log messages

Messages may be logged (at a particular level) by calling one of four methods,
- `Log.fine`
- `Log.info`
- `Log.warn`
- `Log.fail`
passing the single parameter of a `Message` containing the message to be logged.

#### Realm

These four methods require a contextual `Realm` instance in scope so that the source of log messages can be
easily discerned from logs. Conventionally, this would 
be declared in the main application package, and called `Realm`, like so,
```scala
package mylibrary
given Realm: Realm = realm"mylibrary"
```
where `mylibrary` is the name that will appear in the logs. The name `realm` is given
explicitly so that a user-defined `Log` instance may be configured to reference this
realm within the package `mylibrary`, for example:
```scala
given Log = Log.route:
  case myLibrary.Realm => Out
```

Since `given` instances are not imported by default with a wildcard import, a `Realm`
definition does not need to be marked as private.

### Configuration

When working with libraries such as [Scintillate](https://github.com/propensive/scintillate) or
[Guillotine](https://github.com/propensive/guillotine), whose methods require
a `Log` instance, it is possible to selectively include logs from specific libraries,
by referring to that library's realm, for example:
```scala
given Log = Log.route:
  case scintillate.Realm => Out
```
As with `Everything`, a level may also be specified:
```scala
given Log = Log.route:
  case Level.Warn() | Level.Fail() => Err
```

And multiple rules may be included as multiple cases in the `Log` constructor, where the `&` pattern operator
can be used to match on more than one property, for example:
```scala
given Log = Log.route:
  case scintillate.Realm & Info() => Out
  case guillotine.Realm & Warn()  => Err
  case probably.Realm             => Out
```



