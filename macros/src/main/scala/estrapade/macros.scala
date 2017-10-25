package estrapade

import scala.reflect._
import scala.reflect.macros._

object Macros {
  
  /** the implementation of the assertion macro
   *
   *  This macro deconstructs the assertion lambda parameter to an `assert(...)` invocation, in
   *  order to provide improved debugging messages when a test fails. In particular, when asserting
   *  equality or inequality using the `==` and `!=` operators. */
  def assertion(c: blackbox.Context)(assertion: c.Tree)(runner: c.Tree): c.Tree = {
    import c.universe._
   
    // constructs the AST for a new function which creates a failure message based on the test
    // result and the structure of the assertion condition
    val failureMsg = assertion match {
      case Function(param, cond) =>
        val rhs = cond match {
          case q"$left.==($right)" =>
            val diffSearchType = appliedType(typeOf[Diff[_]].typeConstructor, right.tpe)
            val diff = c.inferImplicitValue(diffSearchType, false, false)
            q"""$diff.diff($left, $right)"""

          case q"$left.!=($right)" =>
            q"""$left+" was unexpectedly equal to "+$right"""
          
          case q"$left.contains($right)" =>
            val showSearchType = appliedType(typeOf[Show[_]].typeConstructor, right.tpe)
            val show = c.inferImplicitValue(showSearchType, false, false)
            q"""$show.show{$left}+" did not contain "+$show.show($right)"""
          
          case _ =>
            q""""the assertion was not true""""
        }
        Function(param, rhs)
    }

    q"${c.prefix}.assert($assertion, $failureMsg)"
  }
}

