### Defining a logger

Libraries which use Eucalyptus for logging will need a contextual `Log` instance before they can
be used. This may be as simple as defining a `given Log()` instance in the package where the
libraries are used:
```scala
given Log()
```

This will construct the simplest possible `Log` which logs nothing.

A better alternative may be a logger which logs everything to `STDOUT`,
```scala
given Log(Everything |-> Stdout)
```
or which logs everything at `Warn` level or hight to `STDOUT`:
```scala
given Log(Everything.warn |-> Stdout)
```

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
passing the single parameter of an `AnsiText` containing the message to be logged.

These methods each take a single parameter which is typically an `AnsiText` or a
`Text`, but can be any type that has a given `AnsiShow` instance available.

#### Realm

These four methods require a contextual `Realm` instance. Conventionally, this would 
be declared in the main application package, and called `realm`, like so,
```scala
package mylibrary
given realm: Realm = Realm("mylibrary")
```
where `mylibrary` is the name that will appear in the logs. The name `realm` is given
explicitly so that a user-defined `Log` instance may be configured to reference this
realm within the package `mylibrary`, for example:
```scala
Log(mylibrary.realm |-> Stdout)
```

Since `given` instances are not imported by default with a wildcard import, a `Realm`
definition does not need to be marked as private.

### Configuration

When working with libraries such as [Scintillate](https://github.com/propensive/scintillate) or
[Guillotine](https://github.com/propensive/guillotine), whose methods require
a `Log` instance, it is possible to selectively include logs from specific libraries,
by referring to that library's realm instead of `Everything`, for example:
```scala
given Log(scintillate.realm |-> Stdout)
```
As with `Everything`, a level may also be specified:
```scala
given Log(scintillate.realm.warn |-> Stdout)
```

And multiple rules may be included as repeated arguments to the `Log` constructor, for example:
```scala
given Log(
  scintillate.realm.info |-> Stdout,
  guillotine.realm       |-> Stdout,
  probably.realm.fail    |-> Stdout
)
```
