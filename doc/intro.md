Unlike runtime errors, compilation errors prevent successful compilation, which
makes them harder to test, since we can't even compile the units tests we want
to write and run to test them!

_Larceny_ makes it possible to write those tests. Code which would normally
fail compilation, for any reason (provided it parses as well-formed Scala) is
permitted inside certain blocks of code, but instead of being compiled and run,
instead returns a list of compilation errors, as runtime values, which are
perfect for testing.


