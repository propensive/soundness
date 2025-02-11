Shading a package, effectively renaming it, is one way to avoid conflicts
between packages with the same name. This is a scenario which frequently arises
in complex builts which include transitive dependencies on different versions
of the same library (often called the "diamond dependency problem").

Normally shading is performed on a binary after compilation, but this is slow
and can introduce new problems.  Umbrageous is a compiler plugin which makes it
trivial to compile a library into a package with a distinct prefix without
modifying its source code.

