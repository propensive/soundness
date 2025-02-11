A _lens_ is an object which is able to access and modify specific parts of a
(potentially) complex immutable data structure. It consists of a _getter_ for
access, and a _setter_ for modification. For example, a lens could focus on the
`id` field of a `User` case class; its getter would take an instance of `User`
and return its `id`, while the setter would take an existing `User` instance
and a new `id` value, and return a new instance of `User` with the new `id`,
and all other fields unchanged.

Lenses are notable for their composability. A single lens can focus on a field
inside a case class nested inside another case class, or more deeply nested
fields. In such an example, a single lens, composed from simpler lenses, could
create a new instance of the outermost case class with just one of its
innermost field modified, in a single operation, avoiding potentially very
complex syntax involving making copies of each intermediate case class.

Panopticon provides concise and elegant syntax for working with lenses.
