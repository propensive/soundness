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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import scala.compiletime.*
import scala.quoted.*

import anticipation.*
import contingency.*
import fulminate.*
import gigantism.*
import proscenium.*
import rudiments.*

object protointernal:
  def build(using Quotes)(todo: List[quotes.reflect.TypeRepr]): quotes.reflect.TypeRepr =
    import quotes.reflect.*

    todo match
      case Nil => TypeRepr.of[Zero]

      case next :: todo =>
        next.asType.absolve match
          case '[next] => build(todo).asType.absolve match
            case '[type tupleType <: Tuple; tupleType] => TypeRepr.of[next *: tupleType]

  def decompose(using Quotes)(repr: quotes.reflect.TypeRepr): Set[quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    repr.dealias.asMatchable match
      case AndType(left, right) => decompose(left) ++ decompose(right)
      case other                => Set(other)

  def disintersection[intersection: Type]: Macro[Tuple] =
    import quotes.reflect.*

    build(decompose(TypeRepr.of[intersection].dealias).to(List)).asType.absolve match
      case '[type tupleType <: Tuple; tupleType] => '{null.asInstanceOf[tupleType]}

  def extractor(context: Expr[StringContext]): Macro[Any] =
    import quotes.reflect.*

    val string = context.valueOrAbort.parts.head

    ConstantType(StringConstant(string)).asType match
      case '[type stringType <: Label; stringType] => '{NameExtractor[stringType]()}

      case _ =>
        panic(m"StringContext did not contains Strings")

  def makeName[system: Type](name: Expr[Text]): Macro[Name[system]] =
    import quotes.reflect.*

    Expr.summon[system is Nominative].absolve match
      case Some('{type limit; $nominative: (Nominative { type Limit = limit })}) =>
        val checks =
          decompose(TypeRepr.of[limit]).to(List).map(_.asType).foldLeft('{()}): (expr, next) =>
            next.absolve match
              case '[type param <: String; type rule <: Check[param]; rule] =>
                anteprotointernal.staticCompanion[rule].absolve match
                  case '{$rule: Rule} =>
                    TypeRepr.of[param].absolve match
                      case ConstantType(StringConstant(string)) =>
                        ' {
                            if $rule.check($name, ${Expr(string)}.tt) then $expr
                            else provide[Tactic[NameError]]:
                              raise(NameError($name, $rule, ${Expr(string)}))
                          }

        '{$checks; $name.asInstanceOf[Name[system]]}

      case None =>
        halt(89, m"Couldn't find a `Nominative` instance on ${TypeRepr.of[system].show}")


  def parse2[plane: Type, name <: String: Type](scrutinee: Expr[Name[plane]])
  :   Macro[Boolean] =

    parse[plane, name]
    '{${Expr(constant[name])}.tt == $scrutinee}


  def constant[text <: String: Type](using Quotes): text =
    import quotes.reflect.*

    TypeRepr.of[text].asMatchable.absolve match
      case ConstantType(StringConstant(value)) => value.tt.asInstanceOf[text]

  def companion[companion: Typeable](using Quotes)(symbol: quotes.reflect.Symbol): companion =
    Class.forName(s"${symbol.companionModule.fullName}$$").nn.getField("MODULE$").nn.get(null) match
      case module: `companion` => module
      case _                   => halt(704, m"The companion object did not have the expected type.")

  def parse[plane: Type, name <: String: Type]: Macro[Name[plane]] =
    import quotes.reflect.*

    val name: Text = constant[name].tt

    Expr.summon[plane is Nominative] match
      case
        Some
          ( ' {
                type limit
                type nominative <: Nominative { type Limit = limit }
                $value: nominative
              } ) =>

        decompose(TypeRepr.of[limit]).to(List).each: repr =>
          val text = repr.asMatchable match
            case AppliedType(_, List(param)) =>
              param.asMatchable match
                case ConstantType(StringConstant(text)) => text.tt
                case _                                  => halt(487, m"Bad type")

            case _ =>
              halt(487, m"Bad type")

          val rule = companion[Rule](repr.typeSymbol)

          if !rule.check(name, text)
          then halt(789, m"the name is not valid because it ${rule.describe(text)}")

      case _ =>
        halt(613, m"Could not access constraint")


    '{${Expr(name)}.asInstanceOf[Name[plane]]}
