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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package nomenclature

import anticipation.*
import fulminate.*
import proscenium.*
import rudiments.*

import scala.quoted.*
import scala.compiletime.*

given Realm = realm"nomenclature"

object Nomenclature2:
  def build(using Quotes)(todo: List[quotes.reflect.TypeRepr]): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    todo match
      case Nil          => TypeRepr.of[Zero]
      case next :: todo => next.asType.absolve match
        case '[next] => build(todo).asType.absolve match
          case '[type tupleType <: Tuple; tupleType] => TypeRepr.of[next *: tupleType]

  def decompose(using Quotes)(repr: quotes.reflect.TypeRepr): Set[quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    repr.dealias.asMatchable match
      case AndType(left, right) => decompose(left) ++ decompose(right)
      case other                => Set(other)

  def disintersection[intersection: Type](using Quotes): Expr[Tuple] =
    import quotes.reflect.*

    build(decompose(TypeRepr.of[intersection]).to(List)).asType.absolve match
      case '[type tupleType <: Tuple; tupleType] => '{null.asInstanceOf[tupleType]}

  def extractor(context: Expr[StringContext])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    val string = context.valueOrAbort.parts.head

    ConstantType(StringConstant(string)).asType match
      case '[type stringType <: Label; stringType] => '{NameExtractor[stringType]()}
      case _ =>
        panic(m"StringContext did not contains Strings")

  def parse2[platform: Type, name <: String: Type](scrutinee: Expr[Name[platform]])
     (using Quotes)
  :     Expr[Boolean] =
    parse[platform, name]
    '{${Expr(constant[name])}.tt == $scrutinee.text}

  def constant[text <: String: Type](using Quotes): text =
    import quotes.reflect.*
    TypeRepr.of[text].asMatchable.absolve match
      case ConstantType(StringConstant(value)) => value.tt.asInstanceOf[text]

  def companion[companion: Typeable](using Quotes)(symbol: quotes.reflect.Symbol)
  :     companion =
    Class.forName(s"${symbol.companionModule.fullName}$$").nn.getField("MODULE$").nn.get(null) match
      case module: `companion` => module
      case _                   => halt(m"The companion object did not have the expected type.")

  def parse[platform: Type, name <: String: Type](using Quotes): Expr[Name[platform]] =
    import quotes.reflect.*

    val name: Text = constant[name].tt

    Expr.summon[platform is Nominative] match
      case Some('{ type constraint
                   type nominative <: Nominative { type Constraint = constraint }
                   $value: nominative }) =>
        decompose(TypeRepr.of[constraint]).to(List).each: repr =>
          val text = repr.asMatchable match
            case AppliedType(_, List(param)) => param.asMatchable match
              case ConstantType(StringConstant(text)) => text.tt
              case _                                  => halt(m"Bad type")
            case _                           => halt(m"Bad type")
          val rule = companion[Rule](repr.typeSymbol)
          if !rule.check(name, text)
          then halt(m"the name is not valid because it ${rule.describe(text)}")
      case _ =>
        halt(m"Could not access constraint")


    '{${Expr(name)}.asInstanceOf[Name[platform]]}
