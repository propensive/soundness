## Exceptions in _Scala One_ projects

_Scala One_ projects are designed to take full advantage of Scala's advanced exception-handling, so a
lot of attention is placed on the design of the exception types, when they are thrown, and how they
are displayed. The consistency of these rules helps to build confidence in _Scala One_ libraries.

### Explicit

_Scala One_'s exceptions should always be explicit in their method signatures: if a method may throw a
an `XyzError` then its signature should declare `throws XyzError` as part of its return type. This
allows the full benefits of Scala 3's safer exceptions to be harnessed, while not compromising the
ease of writing "prototype" code with exception-checking turned off.

### Scope

Every _Scala One_ library will throw only exception types defined in that library. That is to say, the
type of the exception (in particular, the package it is in) can be used to determine where it was
thrown.

In general, this means that code in one library which calls a method in another library much catch
its exceptions and throw them as its own. Commonly, the original exception will be captured as the
`cause` of new one.

### Immutable datatypes

Every exception is implemented as a case class with zero or more parameters—all immutable
values—which provide additional information about the circumstances in which the exception was
thrown.

There is often a choice about whether a method should throw an exception, or encode the exceptional
case in a more complex—but pure—datatype. In general, _Scala One_ methods choose exceptions for the cases
where the method may still be composed with others to construct useful software without any
exception handling, in particular when the exception may convey more useful information than
returning `None` instead of `Some`.

### Relevance

The choice of what to include as a parameter depends on a few factors: all parameters should be
helpful to diagnose the exception and should be, at least in theory, values which were critical to
the exception being thrown. When considering if a particular parameter should or should not be
included in the exception's state, if there exist circumstances where a different parameter value
could change whether the exception is thrown or not, then it should be included.

For example, a `time` parameter (the time at which the exception was thrown) could usually not be
justified as a parameter.

### Identifiable

To make it easier to recognize exceptions, their names all end in the word `Error`. Although
`Exception` is already a commonly-used suffix for exception types and Java distinguishes between
subtypes of `Exception` and `Error`, the distinction is rarely used in Scala. So `Error` is used
for succinctness.

Many methods in the same library may throw exceptions, and it is useful to be able to determine
where a particular caught exception originated. Exceptions may be designed to be disambiguated by
having different types, or by inspecting their parameters.

In general, different types are chosen when different code paths are likely to be followed for
recovery; or conversely, the same exception type will be used in multiple locations if the handling
for that exception is likely to be the same, irrespective of where it was thrown. The message text
for any exception type should be broadly the same, modulo its parameters.

Furthermore, even if an exception type is reused, it should be possible to disambiguate between
different instances on the basis of simple, enumerable parameters of that exception. It should never
be necessary to inspect the message text of an exception in order to determine its origine.

### Display

Every exception has a unique name (which is its classname), belongs to a unique library, has a short
one-line description, and also includes a longer explanation. By default, logs will show all this
information except the long explanation. The explanation may additionally be displayed in "debug
mode", along with a full stack trace.

If viewing logs on a terminal, ANSI color codes will be applied to the parts of the message which
are instance-dependent (rather than standard "boilerplate" for that exception).
