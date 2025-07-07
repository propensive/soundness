package repro

import scala.quoted.*

// The trait takes NO type parameter: the anonymous class in the quote below must mention the
// macro's type hole only in its DECLARATIONS (the `Self` alias), never in its parents. If a
// parent mentioned the hole, PickledQuotes' TreeTypeMap would change the ClassInfo and copy the
// class, hiding the bug.
trait TC:
  type Self
  def get: Self

object Macro:
  inline def make[T]: TC { type Self = T } = ${ makeImpl[T] }

  def makeImpl[T: Type](using Quotes): Expr[TC { type Self = T }] =
    '{ new TC { type Self = T; def get: Self = null.asInstanceOf[Self] } }
