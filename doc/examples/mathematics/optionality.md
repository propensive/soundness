# A matrix which may have no inverse

A singular matrix has no inverse, and whether a matrix is singular is not known from its
type. `inverse` gives an optional matrix, so the singular case is met where the inverse is
used, and not as an exception or a matrix of `NaN`s.

```scala fuel
compile
frame
  label An inverse, assumed to exist
  errors show
  code
      val matrix = Matrix[2, 2]((1.0, 2.0), (2.0, 4.0))
      val inverse: Matrix[Double, 2, 2] = matrix.inverse
frame
  label An inverse which may be absent
  highlight Optional[Matrix[Double,..2]]  absent for a singular matrix
  code
      val matrix = Matrix[2, 2]((1.0, 2.0), (2.0, 4.0))
      val inverse: Optional[Matrix[Double, 2, 2]] = matrix.inverse
      val report = inverse.let(_.inspect).or(t"singular")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(report)
```

[Matrices](../../soundness/modules/mathematics.md#matrices) has `solve` and `eigenvalues`,
which are optional for the same reason.
