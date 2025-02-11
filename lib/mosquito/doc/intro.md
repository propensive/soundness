Euclidean vectors, in contrast to scalars and arbitrary collections, represent
values in _a fixed multiple_ number of dimensions. _Mosquito_ provides a
representation of vectors, `Euclidean`, whose generic type encapsulates both
its element type and size. In some sense, a `Euclidean` can be considered a
hybrid of a `Tuple` (whose size in known) and a collection (whose elements are
homogeneous). Mosquito supports the use case of _generic programming_ with
`Euclidean` vectors, but facilitates linear algebra operations, including
working with matrices and scalar and vector products.

