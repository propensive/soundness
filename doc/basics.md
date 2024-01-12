### Example

For example, moving a file from `/home/work/file` to `/home/work/copy` should work fine
if there is no pre-existing file at `/home/work/copy`. We can move it with,
`file.moveTo(destination)`. But if `/home/work/copy` already
exists, then we may or may not care about what happens if we try to overwrite it.

The behavior can be specified with a contextual value in scope. Either,
```scala
import filesystemOptions.overwritePreexisting
```
or,
```scala
import filesystemOptions.doNotOverwritePreexisting
```

The `moveTo` operation does not assume one option or the other as a default, and Galilei's
philosophy is that it would be wrong to do so. Instead, invoking `moveTo` _without_ exactly
one of the two contextal values in scope is a compile error, and the user is forced to
decide on the correct behavior. This is both unpresumptuous and instructive, since the
user may not have even considered the decision had to be made.

As a contextual value, the choice of behavior can be limited to a narrow scope, or
imported globally, as needed.

If Scala 3's "safer exceptions" are turned on, then the choice of behavior also affects
which exceptions must be handled when calling `moveTo`. The method invocation may throw
an `IoError` under any circumstances, so that must always be handled, but with
`doNotOverwritePreexisting` in scope, if there _is_ a pre-existing file at the destination,
then an `OverwriteError` will be thrown, which must be handled.

But since it cannot be thrown with `overwritePreexisting` in scope, the obligation to handle it
is also removed.

### Types

Unlike many disk I/O libraries, __Galilei__ provides different types for `Path`s, `File`s, `Directory`s
and other types of node, like `Symlink`s. A `Path` represents the abstract notion of a location within
a filesystem, which may or may not exist and may be a file, a directory or (on Linux, at least) one of
several other filesystem node types. Types such as `File` and `Directory` should only exist to
correspond to a real file or directory on disk.

Of course, the contents of a filesystem can change independently of the JVM, so the existence of
an immutable `File` or `Directory` instance does not guarantee its eternal existence on disk, but
we do, at least, guarantee that the filesystem node existed and had the correct type at the time
of the object's creation.



