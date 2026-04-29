accesses the annotations on a field of the case class, `:T`, accessed by the lambda, `.fn`

This returns a `:List[StaticAnnotation]` of all annotations on this field of the case class, which may be an
empty list if there are no annotations.

Typically calling this method with a concrete type will not be useful since the annotations will already be
known on a known type. But the field name must nevertheless be known in order to write the lambda to specify
the field. So this method can be most useful when used to access the annotations on a subtype of an interface
which defines the field abstractly.