                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package rudiments

import scala.caps
import scala.quoted.*

import denominative.*
import vacuous.*

// The read on a `Sized` array. A literal ordinal — `156.z`, or one of the ordinal literals
// `Prim` … `Sept`, recognised by symbol — is compared with the proven
// length here, in the macro, so the read is total and an out-of-range literal is a compile
// error. A variable ordinal has no literal to check and takes the ordinary `at` route, which
// yields `Optional`. The types are read from the trees rather than from refinements on the
// ordinal, so an ordinal literal keeps its plain type everywhere else (a refined literal type
// infers into every `val` and `var` that binds it, and defeats exact-type typeclass lookups).
object sizedMacros:
  def read[element: Type, length <: Int: Type]
    ( array:   Expr[Array[element]^{caps.any.rd} & Sized[length]],
      ordinal: Expr[Ordinal] )
    ( using Quotes )
  :   Expr[Any] =

    import quotes.reflect.*

    val length: Int = TypeRepr.of[length].dealias match
      case ConstantType(IntConstant(length)) => length
      case other => report.errorAndAbort(s"the array's proven length ${other.show} is not a literal")

    // The seven ordinal literals are `final val`s (patterns need a stable identifier), so they
    // are known here by symbol rather than by value.
    val ordinalLiterals: Map[String, Int] =
      Map("Prim" -> 0, "Sec" -> 1, "Ter" -> 2, "Quat" -> 3, "Quin" -> 4, "Sen" -> 5, "Sept" -> 6)

    def named(symbol: Symbol): Optional[Int] =
      if symbol.owner.fullName.startsWith("denominative") then ordinalLiterals.at(symbol.name)
      else Unset

    def literal(term: Term): Optional[Int] = term match
      case Inlined(_, _, inner)        => literal(inner)
      case Typed(inner, _)             => literal(inner)
      case Block(Nil, inner)           => literal(inner)
      case Literal(IntConstant(value)) => value
      case ident @ Ident(_)            => named(ident.symbol)
      case select @ Select(_, _)       => named(select.symbol)
      case _                           => Unset

    literal(ordinal.asTerm).lay('{ ($array: Array[element]^{caps.any.rd}).at($ordinal) }): index =>
      if index >= 0 && index < length
      then '{ ($array: Array[element]^{caps.any.rd}).readUnchecked($ordinal.n0) }
      else report.errorAndAbort(s"the ordinal $index is beyond the array's proven length of $length")
