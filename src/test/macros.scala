package hyperbole

import scala.quoted.*

object Macros:
  def impl[T](expr: Expr[T])(using Quotes): Expr[T] =
    import quotes.*, reflect.*
    import reflection.*
    report.info(expand(expr).s)
    expr

  transparent inline def inspect[T](inline value: T): Unit = ${Macros.impl[T]('value)}