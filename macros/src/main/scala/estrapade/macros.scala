package estrapade

import scala.reflect._
import scala.reflect.macros._

object Macros {
  
  /** the implementation of the assertion macro
   *
   *  This macro deconstructs the assertion lambda parameter to an `assert(...)` invocation, in
   *  order to provide improved debugging messages when a test fails. In particular, when asserting
   *  equality or inequality using the `==` and `!=` operators
   *
   *  This implementation */
  def assertion(c: blackbox.Context)(assertion: c.Tree)(runner: c.Tree): c.Tree = {
    import c.universe._
   
    val failureMsg = assertion match {
      case Function(param, cond) =>
        val rhs = cond match {
          case q"$left.==($right)" =>
            val diffSearchType = appliedType(typeOf[Diff[_]].typeConstructor, right.tpe)
            scala.util.Try(c.inferImplicitValue(diffSearchType, false, false)).toOption match {
              case None =>
                val rightShowSearchType = appliedType(typeOf[Show[_]].typeConstructor, right.tpe)
                val rightShowImplicit = c.inferImplicitValue(rightShowSearchType, false, false)
                val leftShowSearchType = appliedType(typeOf[Show[_]].typeConstructor, left.tpe)
                val leftShowImplicit = c.inferImplicitValue(leftShowSearchType, false, false)
                q"""$leftShowImplicit.show($left)+" did not equal "+$rightShowImplicit.show($right)"""
              case Some(diff) =>
                q"""$diff.diff($left, $right)"""
            }

          case q"$left.!=($right)" => q"""$left+" was unexpectedly equal to "+$right"""
          case q"$left.contains($right)" => q"""$left+" did not contain "+$right"""
          case _ => q""""the assertion was not true""""
        }
        Function(param, rhs)
    }

    q"${c.prefix}.assertMessage($assertion, $failureMsg)"
  }
}

