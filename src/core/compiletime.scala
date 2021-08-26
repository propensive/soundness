package probably

import scala.quoted.*, staging.*

object Check:
  inline def apply[T](code: Quotes ?=> Expr[T])(using Compiler) =
    staging.withQuotes(code)