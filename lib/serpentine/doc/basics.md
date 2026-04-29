The type `Root` defines a "root", beneath which any number of possible `Path` instances may exist in
a hierarchy, and the methods `parent`, `ancestor` and `/` may be used to navigate between them. For
abstract paths, the value `Base` can serve as a root node for a path hierarchy.

Here are some examples:
```scala
val crustaceans = Base / "eukaryota" / "animalia" / "arthropods" / "crustaceans"
val arthropods = crustaceans.parent
val animalia = crustaceans.ancestor(2)
```

`Path`s may be further distinguished as `Path.Relative` or `Path.Absolute`, where a `Relative` may
be converted into an `Absolute` by calling the `Path#absolute` method, and passing an absolute path
to which the relative path should be considered relative to. The result is typed as `Path.Absolute`.

`Path` objects, whether absolute or relative, serialize with `toString` according to the delimiters
in their root, which are defined in terms of a base name (for example, `/` or `classpath:`) and a
separator (for example, `\`, `/` or `.`).

Any implementation of `Root` should define these values, `prefix` and `separator`, respectively. For
example, the definition,
```scala
object Domain extends Root(prefix = "", separator = ".")
```
would ensure that,
```scala
Domain / "www" / "example" / "com"
```
would serialize to the string `"www.example.com"`.

Note that the `separator` is not included between the `prefix` and the first path element when
serializing, so _may_ need to be included in the `prefix` value itself.

### Other Methods

The method `++` can add a `Relative` path to an `Absolute` path, and return a new `Absolute` path.

Similarly, `Absolute#relativeTo` takes another `Absolute` path and returns a `Relative` instance
that, when applied with `++` to the first path, produces the second path.

The `Absolute#conjunction` method will find the closest common parent of the path and its parameter.

Note that `Path`s are not aware of their children, so there is no `children` method, but this may be
provided by individual implementations.

### Exceptions

Many operations on `Path`s may attempt (directly or indirectly) to access the parent of the root.
This is not possible, and if this happens, a `RootBoundaryExceeded` exception will be thrown.

### Generic relative paths

Given that a relative path is (by definition) not attached to any particular root, all instances of
`Root#Path.Relative` inherit from `GenericRelative` which gives users the choice, when implementing
APIs that accept relative paths, between accepting _any_ kind of relative path (regardless of its
origin) and accepting just those originating from a particular root.



