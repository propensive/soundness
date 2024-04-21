When compiling code that depends on _Soundness_, several compiler flags are recommended.

The following provide important semantic benefits in code quality:
 - `-language:experimental.saferExceptions`—to ensure that exceptions are checked, or at least
   that the import `unsafeExceptions.canThrowAny` is in scope
 - `-Yexplicit-nulls`—to forbid the use of `null`
 - `-Ycheck-all-patmat`—to fail on pattern matches which may not match every case, and can thus
   throw a `MatchError` at runtime
 - `-Ysafe-init`—to check that template body members are guaranteed to be initialized before
   they are used

Additionally, these flags may be useful:
 - `-deprecation`—to see warnings about APIs that are are not future-proof
 - `-Werror`—to convert warnings into errors that fail the build
 - `-Wunused:all`—to be notified when an import is not used
 - `-new-syntax`—to enforce the use of the latest Scala 3 syntax, and reject Scala-2-style
   constructs
 - `-Yrequire-targetName`—to ensure that every symbolic name is provided with an alphanumeric
   alternative
 - `-Xverify-signatures`—to provide some additional checking after bytecode generation; however,
   it is not entirely clear what benefits this provides
 - `-Xcheck-macros`—to perform some additional checks on macros
 - `-feature`—to be notified of feature warnings
