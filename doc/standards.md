## Propensive Coding Standards

All Propensive code should conform to the a set of coding standards.

### No long lines

No line should be longer than 112 characters.

This can be checked by a future Pedant compiler plugin.

### Capture checking

Scala 3's experimental Capture Checking feature should be enabled.

This can be enforced by a global compiler flag.

### Separate sources

Different concepts should be kept in different source files.

This must be checked manually.

### Naming conventions

All non-private term and type entities should be given meaningful and consistent names, according to Soundness's naming conventions.

This must be checked manually.

### Significant whitespace

All source code should use significant-whitespace syntax.

### No warnings

Code should compile with zero warnings.

This can be enforced by the compiler, by making warnings fatal.

### Full Test Coverage

All code should have 100% test coverage, unless there is a good reason not to.

This will be possible to check in a later version of Fury and Probably.

### No unused imports

Every import should be required for the source file it is in.

This can be checked by the compiler with the `-Wunused` flags.

### `implicitNotFound` Annotations

Every type which might be requested as a contextual value should have an appropriate `implicitNotFound` annotation.

This can be checked with a future Pedant compiler plugin.

### Avoid blacklisted methods

Methods in the Scala or Java standard libraries which throw exceptions should not be used, except where those exceptions are handled.

This can be checked with a future Pedant compiler plugin.
