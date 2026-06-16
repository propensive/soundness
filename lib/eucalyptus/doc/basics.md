### Defining a logger

Libraries which use Eucalyptus for logging will need a contextual `Log` instance before they can
be used. This may be as simple as defining a `given Log` instance in the package where the
libraries are used, for example,
```scala
given Log = logging.silentLogging
```
or,
```scala
given Log = logging.stdoutLogging
```
or,
```scala
given Log = logging.syslogLogging
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

### Configuration

A level may be specified when routing log entries:
```scala
given Log = Log.route:
  case Level.Warn() | Level.Fail() => Err
```



