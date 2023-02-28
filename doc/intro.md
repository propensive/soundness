Unlike runtime errors, compilation errors prevent successful compilation, which
makes them harder to test since we can't include them in ordinary unit tests,
which must be compiled in order to be run.

_Larceny_ takes a different approach. Code which would normally fail
compilation, for any reason (provided it parses as well-formed Scala) is
permitted inside certain blocks of code, but instead of being compiled and run,
instead returns a list of compilation errors, as runtime values, which are
perfect for testing.

