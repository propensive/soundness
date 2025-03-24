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
package vicarious

import proscenium.*
import rudiments.*

import scala.compiletime.*
import scala.quoted.*

object Vicarious:
  def catalog[KeyType: Type, value: Type]
     (lambda: Expr[[FieldType] => (field: FieldType) => value],
      value: Expr[KeyType],
      classTag: Expr[ClassTag[value]])
     (using Quotes)
  :     Expr[Catalog[KeyType, value]] =
    import quotes.reflect.*

    def fields[ProductType: Type](term: Term): List[Term] =
      TypeRepr.of[ProductType].typeSymbol.caseFields.flatMap: field =>
        term.select(field).asExpr.absolve match
          case '{ $field: fieldType } =>
            '{$lambda[fieldType]($field)}.asTerm :: fields[fieldType](field.asTerm)

    '{ given ClassTag[value] = $classTag
       Catalog(IArray(${Varargs(fields[KeyType](value.asTerm).map(_.asExprOf[value]))}*))  }

  def fieldNames[ProductType: Type](prefix: String)(using Quotes): List[String] =
    import quotes.reflect.*
    TypeRepr.of[ProductType].typeSymbol.caseFields.flatMap: field =>
      val label = if prefix == "" then field.name else prefix+"."+field.name
      field.info.asType.absolve match
        case '[fieldType] => label :: fieldNames[fieldType](label)

  def dereference[KeyType: Type, value: Type, IdType <: Nat: Type]
     (key: Expr[String])(using Quotes)
  :     Expr[value | Proxy[KeyType, value, Nat]] =

    import quotes.reflect.*

    val index = TypeRepr.of[IdType].asMatchable.absolve match
      case ConstantType(IntConstant(index)) => index

    val fields = fieldNames[KeyType]("")

    val label = fields(index)+"."+key.valueOrAbort
    ConstantType(IntConstant(fields.indexOf(label))).asType.absolve match
      case '[ type idType <: Nat; idType ] => '{Proxy[KeyType, value, idType]()}

  def proxy[KeyType: Type, value: Type](using Quotes): Expr[Proxy[KeyType, value, 0]] =
    import quotes.reflect.*

    val fields = fieldNames[KeyType]("")

    def recur(prefix: String, repr: TypeRepr): TypeRepr =
      val index: Int = if prefix == "" then 0 else fields.indexOf(prefix)
      val nat = ConstantType(IntConstant(index))

      val base =
        TypeRepr.of[Proxy].appliedTo(List(TypeRepr.of[KeyType], TypeRepr.of[value], nat))

      repr.typeSymbol.caseFields.fuse(base):
        val label = if prefix == "" then next.name else prefix+"."+next.name
        val fieldType: TypeRepr = next.info
        Refinement(state, next.name, recur(label, fieldType))

    recur("", TypeRepr.of[KeyType]).asType.absolve match
      case '[type proxyType <: Proxy[KeyType, value, 0]; proxyType] =>
        '{Proxy().asInstanceOf[proxyType]}
