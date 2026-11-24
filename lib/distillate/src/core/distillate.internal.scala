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
package distillate

import scala.quoted.*
import scala.reflect

import anticipation.*
import fulminate.*
import gigantism.*

object internal:
  def strictFailure[value: Type](format: Expr[String])(using Quotes): Expr[Nothing] =
    import quotes.reflect.*

    val name = TypeRepr.of[value].dealias.show

    val library = format.valueOrAbort.tt

    halt:
      m"$library: ${name.tt} has no direct codec, and `codecs.strict` does not take the derived one"

  def enumerable[enumeration <: reflect.Enum: Type]: Macro[enumeration is Enumerable] =
    import quotes.reflect.*

    val symbol = TypeRepr.of[enumeration].typeSymbol
    val companion = Ref(symbol.companionModule).asExpr

    // The cases' names are the texts a value is found by, unless the enumeration says
    // otherwise by overriding `toString`; then the names are only known by asking each value.
    val renamed = symbol.declaredMethods.exists(_.name == "toString")

    val cases: scala.collection.immutable.List[(Int, String)] =
      if renamed then scala.collection.immutable.Nil
      else symbol.children.map(_.name).zipWithIndex.map(_.swap)

    ' {
        new Enumerable:
          type Self = enumeration
          val name: Text = ${Expr(TypeRepr.of[enumeration].show)}.tt

          override def value(name: Text): vacuous.Optional[enumeration] =
            val text: String = name.s

            $ {
                val hashed = PerfectHash.dispatch(cases, 'text, Expr(-1))

                if hashed == null then '{indexed(name)} else '{numbered($hashed)}
              }

          val values: Array[enumeration]^{} =
            $ {
                companion.absolve match
                  case '{$companion: companion} =>
                    val ref =
                      TypeRepr.of[companion].typeSymbol.declaredMethod("values")
                      . headOption
                      . getOrElse:
                          halt(m"enum ${TypeRepr.of[enumeration].show} is not a simple choice")

                    companion.asTerm.select(ref).asExprOf[scala.Array[enumeration]]
              }

            . asInstanceOf[Array[enumeration]^{}]
      }
