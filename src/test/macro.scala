package guillotine

import scala.quoted.*, staging.*

object Check:
  def doCheck(str: Expr[Seq[String]])(using Quotes): Expr[Command] = '{StringContext($str*).sh()}
  inline def compile(inline string: String*): Command = ${doCheck('string)}
  inline def apply(inline string: String*)(using Compiler): Command = staging.withQuotes(compile(string*))