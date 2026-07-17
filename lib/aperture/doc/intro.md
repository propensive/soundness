Many entities share the same shape of access: a value describes _where_ something is — a path, an
in-memory buffer — and gaining access to its contents is a distinct, scoped act of _opening_ it.
Aperture defines the `Openable` typeclass and a universal `open` method to capture this pattern
once: a target is opened in some _form_ (the same path might open as a file, a directory or a ZIP
archive), in a _mode_ determining the operations granted inside the scope, with flags
particular to the kind of target. The handle provided within the scope is a capability, confined
to it by capture checking, and its type reflects exactly the grants that were requested — so code
which writes requires a handle that was opened for writing, checked at compile time.
