package probably

import scala.quoted.*

object AltMacros:
  import scala.reflect.*
  def assert[T: Type](runner: Expr[Runner], pred: Expr[T => Boolean])(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    '{ () }