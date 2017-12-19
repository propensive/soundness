package probation

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
  
    def findImplicit(typ: Type, left: Type, right: Type): Tree = {
      val searchType = appliedType(typ.typeConstructor, lub(List(left, right)))
      c.inferImplicitValue(searchType, false, false)
    }

    // constructs the AST for a new function which creates a failure message based on the test
    // result and the structure of the assertion condition
    val failureMsg = assertion match {
      case Function(param, cond) =>
        val rhs = cond match {
          case q"$left.==($right)" =>
            val diff = findImplicit(typeOf[Diff[_]], left.tpe, right.tpe)
            q"""$diff.diff($left, $right)"""

          case q"$left.!=($right)" =>
            val show = findImplicit(typeOf[Show[_]], left.tpe, right.tpe)
            q"""$show.show($left)+" was unexpectedly equal to "+$show.show($right)"""
          
          case q"$left.contains($right)" =>
            val show = findImplicit(typeOf[Show[_]], left.tpe, right.tpe)
            q"""$show.show{$left}+" did not contain "+$show.show($right)"""
          
          case _ =>
            q""""the assertion was not true""""
        }
        Function(param, rhs)
    }

    q"${c.prefix}.assert($assertion, $failureMsg)"
  }
}

