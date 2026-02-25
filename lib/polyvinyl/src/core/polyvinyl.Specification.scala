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
package polyvinyl

import scala.quoted.*

import anticipation.*
import fulminate.*
import prepositional.*
import proscenium.*

trait Specification extends Original:
  type Origin
  protected type Origin0 = Origin
  type Form <: { type Origin = Origin0 }

  def fields: Map[Text, Member]
  def make(data: Origin, transform: Text => Origin => Any): Record
  def access(name: Text, value: Origin): Origin

  def build(value: Expr[Origin])(using Type[Origin], Type[Form])(using thisType: Type[this.type])
  :   Macro[Record] =

    import quotes.reflect.*

    given realm: Realm = realm"polyvinyl"

    val target = thisType.absolve match
      case '[thisType] =>
        Ref(TypeRepr.of[thisType].typeSymbol.companionModule)
        . asExprOf[Specification in Form from Origin]


    def refine
      ( value:    Expr[Origin],
        fields:   List[(Text, Member)],
        refined:  TypeRepr,
        caseDefs: List[CaseDef] = List(CaseDef(Wildcard(), None, '{???}.asTerm)) )
    :   (TypeRepr, List[CaseDef]) =

        fields match
          case Nil =>
            (refined, caseDefs)

          case (name, Member.Value(label, params*)) :: tail =>
            ConstantType(StringConstant(label.s)).asType.absolve match
              case '[type label <: Label; label] =>
                Expr.summon[label is Intensional in Form from Origin].absolve match
                  case None =>
                    halt:
                      m"""
                        could not find an Intensional instance for the field $name with type $label
                      """

                  case Some
                    ( '{$accessor: label `is` Intensional `in` Form `from` Origin `to` result} ) =>

                    val rhs: Expr[Origin => Any] =
                      ' {
                          (data: Origin) =>
                            $accessor.transform
                              ( $target.access(${Expr(name)}, data), ${Expr(params.to(List))} )
                        }

                    val caseDefs2 =
                      CaseDef(Literal(StringConstant(name.s)), None, rhs.asTerm) :: caseDefs

                    val refinement = Refinement(refined, name.s, TypeRepr.of[result])

                    refine(value, tail, refinement, caseDefs2)

          case (name, Member.Record(label, map)) :: tail =>
            ConstantType(StringConstant(label.s)).asType.absolve match
              case '[type label <: Label; label] =>
                Expr.summon[label is Structural[?] in Form from Origin].absolve match
                  case None =>
                    halt:
                      m"""
                        could not find a Structural instance for the field $name with type $label
                      """

                  case Some
                    ( ' {
                          type constructor[_]
                          $structural: (label `is` Structural[constructor] `in` Form `from` Origin)
                        } ) =>

                    val nested = '{$target.access(${Expr(name)}, $value)}
                    val recordTypeRepr = TypeRepr.of[Record]
                    val (nestedType, nestedCaseDefs) = refine(nested, map.to(List), recordTypeRepr)

                    val matchFn: Expr[Text => Origin => Any] =
                      ' {
                          (name: Text) =>
                            ${Match('name.asTerm, nestedCaseDefs).asExprOf[Origin => Any]}
                        }

                    val maker: Expr[Origin => Record] = '{field => $target.make(field, $matchFn)}

                    val rhs: Expr[Origin => Any] =
                      '{data => $structural.transform($target.access(${Expr(name)}, data), $maker)}

                    val caseDef = CaseDef(Literal(StringConstant(name.s)), None, rhs.asTerm)

                    nestedType.asType.absolve match
                      case '[nestedRecordType] =>
                        val typeRepr = TypeRepr.of[constructor[nestedRecordType]]

                        refine
                          ( value,
                            tail,
                            Refinement(refined, name.s, typeRepr),
                            caseDef :: caseDefs )


    val (refined, caseDefs) = refine(value, fields.to(List), TypeRepr.of[Record])

    val matchFn: Expr[Text => Origin => Any] =
      '{(name: Text) => ${Match('name.asTerm, caseDefs).asExprOf[Origin => Any]}}

    refined.asType.absolve match
      case '[type refined <: Record; refined] =>
        '{$target.make($value, $matchFn).asInstanceOf[refined]}
