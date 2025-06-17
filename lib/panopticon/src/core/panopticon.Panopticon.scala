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
┃    Soundness, version 0.35.0.                                                                    ┃
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
package panopticon

import anticipation.*
import fulminate.*
import proscenium.*

import scala.quoted.*
import scala.compiletime.*

import language.dynamics

object Panopticon:
  given realm: Realm = realm"panopticon"
  opaque type Lens[from, path <: Tuple, to] = Int
  opaque type InitLens[from] = Int

  object Lens:
    def apply[from]: InitLens[from] = 0
    def make[from, path <: Tuple, to](): Lens[from, path, to] = 0

  extension [from](initLens: InitLens[from])
    def apply[path <: Tuple, to](lambda: Aim[from, Zero] => Aim[to, path]): Lens[from, path, to] =
      0

  extension [from, path <: Tuple, to](lens: Lens[from, path, to])
    @targetName("append")
    infix def ++ [to2, path2 <: Tuple](right: Lens[to, path2, to2])
    : Lens[from, Tuple.Concat[path, path2], to2] =

        Lens.make()


    inline def apply(aim: from): to = ${Panopticon.get[from, path, to]('aim)}

    inline def update(aim: from, newValue: to): from =
      ${Panopticon.set[from, path, to]('aim, 'newValue)}

  private def getPath[tuple <: Tuple: Type](path: List[String] = Nil)(using Quotes): List[String] =
    import quotes.reflect.*

    Type.of[tuple] match
      case '[type tail <: Tuple; head *: tail] =>
        TypeRepr.of[head].asMatchable.absolve match
          case ConstantType(StringConstant(str)) => getPath[tail](str :: path)

      case _ =>
        path


  def getPaths[tuple <: Tuple: Type](paths: List[List[String]] = Nil)(using Quotes)
  : List[List[String]] =

      Type.of[tuple] match
        case '[type tail <: Tuple; head *: tail] =>
          Type.of[head].absolve match
            case '[type tupleType <: Tuple; tupleType] =>
              getPath[tupleType]() :: getPaths[tail]()

        case _ =>
          halt(m"unexpectedly did not match")


  def get[from: Type, path <: Tuple: Type, to: Type](value: Expr[from]): Macro[to] =

    import quotes.reflect.*

    def select[aim: Type](path: List[String], expr: Expr[aim]): Expr[to] =
      path match
        case Nil          => expr.asExprOf[to]
        case next :: tail => ConstantType(StringConstant(next)).asType.absolve match
          case '[type next <: Label; next] =>
            Expr.summon[Dereferencer[aim, next]] match
              case Some('{ type field
                           $dereferencer: Dereferencer[aim, label] {  type Field = field  } }) =>
                select[field](tail, '{$dereferencer.field($expr)})

              case _ =>
                val aimSymbol = TypeRepr.of[aim].typeSymbol

                expr.asTerm.select(aimSymbol.fieldMember(next)).asExpr.absolve match
                  case '{$expr: aimType} => select[aimType](tail, expr)

    select[from](getPath[path](), value).asExprOf[to]


  def set[from: Type, path <: Tuple: Type, to: Type](value: Expr[from], newValue: Expr[to])
  : Macro[from] =

      import quotes.reflect.*

      val fromTypeRepr: TypeRepr = TypeRepr.of[from]

      def rewrite(path: List[String], term: Term): Term =
        path match
          case Nil =>
            term

          case next :: tail =>
            val newParams = term.tpe.typeSymbol.caseFields.map: field =>
              if field.name == next then
                if tail == Nil then newValue.asTerm else rewrite(tail, Select(term, field))
              else Select(term, field)

            term.tpe.classSymbol match
              case Some(classSymbol) =>
                Apply
                 (Select(New(TypeIdent(classSymbol)), term.tpe.typeSymbol.primaryConstructor),
                  newParams)

              case None =>
                halt(m"the type $fromTypeRepr does not have a primary constructor")

      rewrite(getPath[path](), value.asTerm).asExprOf[from]


  def dereference[aim: Type, tuple <: Tuple: Type](member: Expr[String]): Macro[Any] =
    import quotes.reflect.*

    val fieldName = member.valueOrAbort
    val fieldNameType = ConstantType(StringConstant(fieldName)).asType
    val aimType = TypeRepr.of[aim]

    fieldNameType.absolve match
      case '[type fieldName <: Label; fieldName] =>
        Expr.summon[Dereferencer[aim, fieldName]] match
          case Some('{ type field
                       $dereferencer: Dereferencer[aim, label] { type Field = field } }) =>
            '{Aim[field, fieldName *: tuple]()}

          case _ =>
            aimType.typeSymbol.caseFields.find(_.name == fieldName) match
              case None =>
                halt(m"the field $fieldName is not a member of $aimType")

              case Some(symbol) => symbol.info.asType.absolve match
                case '[result] => '{Aim[result, fieldName *: tuple]()}
