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

trait Intension[data, record <: Record in data]:
  def fields: Map[Text, RecordField]
  def make(data: data, transform: Text => data => Any): record
  def access(name: Text, value: data): data

  def build(value: Expr[data])(using Type[record], Type[data])(using thisType: Type[this.type])
  :   Macro[record] =

    import quotes.reflect.*

    given realm: Realm = realm"polyvinyl"

    val target = thisType.absolve match
      case '[thisType] =>
        Ref(TypeRepr.of[thisType].typeSymbol.companionModule).asExprOf[Intension[data, record]]


    def refine
      ( value:    Expr[data],
        fields:   List[(Text, RecordField)],
        refined:  TypeRepr,
        caseDefs: List[CaseDef] = List(CaseDef(Wildcard(), None, '{???}.asTerm)) )
    :   (TypeRepr, List[CaseDef]) =

        fields match
          case Nil =>
            (refined, caseDefs)

          case (name, RecordField.Value(identifier, params*)) :: tail =>
            ConstantType(StringConstant(identifier.s)).asType.absolve match
              case '[type identifier <: Label; identifier] =>
                Expr.summon[identifier is Intensional in data on record].absolve match
                  case None =>
                    halt:
                      m"""
                        could not find an Intensional instance for the field $name with type
                        $identifier
                      """

                  case Some('{$accessor: Intensional { type Self = identifier; type Form = `data`; type Result = valueType; type Plane = `record` }}) =>

                    val rhs: Expr[data => Any] =
                      '{
                        (data: data) =>
                          $accessor.transform
                            ( $target.access(${Expr(name)}, data), ${Expr(params.to(List))} )
                      }

                    val caseDefs2 =
                      CaseDef(Literal(StringConstant(name.s)), None, rhs.asTerm) :: caseDefs

                    val refinement = Refinement(refined, name.s, TypeRepr.of[valueType])

                    refine(value, tail, refinement, caseDefs2)

          case (name, RecordField.Record(identifier, map)) :: tail =>
            ConstantType(StringConstant(identifier.s)).asType.absolve match
              case '[type identifier <: Label; identifier] =>
                Expr.summon[Accessor[record, data, identifier, ?]].absolve match
                  case None =>
                    halt:
                      m"""
                        could not find an Accessor instance for the field $name with type
                        $identifier
                      """

                  case Some
                    ( ' {
                          type constructor[_]
                          $accessor: Accessor[`record`, `data`, identifier, constructor]
                        } ) =>

                    val nested = '{$target.access(${Expr(name)}, $value)}
                    val recordTypeRepr = TypeRepr.of[record]

                    val (nestedType, nestedCaseDefs) =
                      refine(nested, map.to(List), recordTypeRepr)

                    val matchFn: Expr[Text => data => Any] =
                      ' {
                          (name: Text) =>
                            ${Match('name.asTerm, nestedCaseDefs).asExprOf[data => Any]}
                        }

                    val maker: Expr[data => record] = '{field => $target.make(field, $matchFn)}

                    val rhs: Expr[data => Any] =
                      '{data => $accessor.transform($target.access(${Expr(name)}, data), $maker)}

                    val caseDef = CaseDef(Literal(StringConstant(name.s)), None, rhs.asTerm)

                    nestedType.asType.absolve match
                      case '[nestedRecordType] =>
                        val typeRepr = TypeRepr.of[constructor[nestedRecordType]]

                        refine
                          ( value,
                            tail,
                            Refinement(refined, name.s, typeRepr),
                            caseDef :: caseDefs )


    val (refined, caseDefs) = refine(value, fields.to(List), TypeRepr.of[record])

    val matchFn: Expr[Text => data => Any] =
      '{(name: Text) => ${Match('name.asTerm, caseDefs).asExprOf[data => Any]}}

    refined.asType.absolve match
      case '[type refined <: record; refined] =>
        '{$target.make($value, $matchFn).asInstanceOf[refined]}
