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
  def fields: Map[String, RecordField]
  def make(data: data, transform: String => data => Any): record
  def access(name: String, value: data): data


  def build(value: Expr[data])(using Type[record], Type[data])(using thisType: Type[this.type])
  : Macro[record] =

      import quotes.reflect.*

      given realm: Realm = realm"polyvinyl"

      val target = thisType.absolve match
        case '[thisType] =>
          Ref(TypeRepr.of[thisType].typeSymbol.companionModule).asExprOf[Intension[data, record]]


      def refine
        ( value:       Expr[data],
          fields:      List[(String, RecordField)],
          refinedType: TypeRepr,
          caseDefs:    List[CaseDef]               = List(CaseDef(Wildcard(), None, '{???}.asTerm)) )
      : (TypeRepr, List[CaseDef]) =

          fields match
            case Nil =>
              (refinedType, caseDefs)

            case (name, RecordField.Value(typeName, params*)) :: tail =>
              ConstantType(StringConstant(typeName)).asType.absolve match
                case '[type typeName <: Label; typeName] =>
                  Expr.summon[Intensional[record, data, typeName, ?]].absolve match
                    case None =>
                      halt:
                        m"""could not find a Intensional instance for the field $name with type
                            $typeName"""

                    case Some('{$accessor: Intensional[`record`, `data`, typeName, valueType]}) =>

                      val rhs: Expr[data => Any] =
                        '{
                          (data: data) =>
                            $accessor.transform
                            ($target.access(${Expr(name)}, data), ${Expr(params.to(List))})
                        }

                      val caseDefs2 =
                        CaseDef(Literal(StringConstant(name)), None, rhs.asTerm) :: caseDefs

                      val refinement = Refinement(refinedType, name, TypeRepr.of[valueType])

                      refine(value, tail, refinement, caseDefs2)

            case (name, RecordField.Record(typeName, map)) :: tail =>
              ConstantType(StringConstant(typeName)).asType.absolve match
                case '[type typeName <: Label; typeName] =>
                  Expr.summon[RecordAccessor[record, data, typeName, ?]].absolve match
                    case None =>
                      halt(m"""could not find a RecordAccessor instance for the field $name with
                            type $typeName""")

                    case Some('{ type typeConstructor[_]
                                $accessor: RecordAccessor
                                            [`record`, `data`, typeName, typeConstructor] }) =>

                      val nested = '{$target.access(${Expr(name)}, $value)}
                      val recordTypeRepr = TypeRepr.of[record]

                      val (nestedType, nestedCaseDefs) =
                        refine(nested, map.to(List), recordTypeRepr)

                      val matchFn: Expr[String => data => Any] =
                        '{ (name: String) =>
                            ${Match('name.asTerm, nestedCaseDefs).asExprOf[data => Any]} }

                      val maker: Expr[data => record] =
                        '{ field => $target.make(field, $matchFn) }

                      val rhs: Expr[data => Any] =
                        '{ data =>
                             $accessor.transform($target.access(${Expr(name)}, data), $maker) }

                      val caseDef = CaseDef(Literal(StringConstant(name)), None, rhs.asTerm)

                      nestedType.asType.absolve match
                        case '[nestedRecordType] =>
                          val typeRepr = TypeRepr.of[typeConstructor[nestedRecordType]]

                          refine
                           (value,
                            tail,
                            Refinement(refinedType, name, typeRepr),
                            caseDef :: caseDefs)


      val (refinedType, caseDefs) = refine(value, fields.to(List), TypeRepr.of[record])

      val matchFn: Expr[String => data => Any] =
        '{(name: String) => ${Match('name.asTerm, caseDefs).asExprOf[data => Any]}}

      refinedType.asType.absolve match
        case '[type refinedType <: record; refinedType] =>
          '{$target.make($value, $matchFn).asInstanceOf[refinedType]}
