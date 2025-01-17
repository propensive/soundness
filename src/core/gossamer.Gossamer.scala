package gossamer

import anticipation.*
import denominative.*
import fulminate.*
import rudiments.*
import spectacular.*
import vacuous.*

import scala.quoted.*

object Gossamer:
  given Realm = realm"gossamer"

  def ascii(context: Expr[StringContext], parts: Expr[Seq[String]])(using Quotes): Expr[Ascii] =
    val dynamicParts = (parts: @unchecked) match
      case Varargs(parts) => parts

    val staticParts = context.value.get.parts.map: part =>
      part.tt.chars.each: char =>
        if char >= 128 then halt(m"$char is not a valid ASCII character")

      Expr[String](part)

    def recur(first: List[Expr[String]], second: List[Expr[String]], expr: Expr[String])
            : Expr[String] =
      first match
        case head :: tail => recur(second, tail, '{$expr+$head})
        case Nil          => expr

    '{  val string = ${recur(staticParts.tail.to(List), dynamicParts.to(List), staticParts.head)}
        Ascii(string.getBytes("ASCII").nn.immutable(using Unsafe))  }
