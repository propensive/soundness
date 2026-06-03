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
package adversaria

import java.lang as jl

import scala.quoted.*

import anticipation.*
import denominative.*
import fulminate.*
import gigantism.*
import prepositional.*
import rudiments.*
import vacuous.*

object internal:
  private def rebuild(using Quotes)(term: quotes.reflect.Term): Optional[quotes.reflect.Term] =
    import quotes.reflect.*

    object Mapper extends TreeMap:
      override def transformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree =
        tree match
          case ident: TypeIdent => TypeIdent(tree.symbol)

          // A type-parameterized annotation (e.g. `@label[Xml](…)`) stores its
          // type as an applied type tree. Re-root the type constructor and
          // synthesise fresh argument trees from their types — the originals are
          // loaded from another compilation unit's TASTy without usable source
          // positions, which the later `typedTypeApply` asserts are present.
          case Applied(constructor, arguments) =>
            val arguments2 = arguments.map:
              case argument: TypeTree => TypeTree.of(using argument.tpe.asType)
              case argument           => argument

            Applied(transformTypeTree(constructor)(owner), arguments2)

          case _ => throw jl.Error()

      override def transformTerm(tree: Term)(sym: Symbol): Term =
        tree match
          case Inlined(_, Nil, expr)   => transformTerm(expr)(sym)
          case Ident(id)               => Ident(tree.symbol.termRef)
          case Select(qualifier, name) => Select(transformTerm(qualifier)(sym), tree.symbol)
          case New(tpt)                => New(transformTypeTree(tpt)(sym))
          case Literal(constant)       => Literal(constant)

          case Apply(fn, arguments) =>
            Apply(transformTerm(fn)(sym), transformTerms(arguments)(sym))

          // The constructor of a type-parameterized annotation is applied to its
          // type arguments before its value arguments. Synthesise fresh type-arg
          // trees from their types (see `transformTypeTree` above).
          case TypeApply(fn, arguments) =>
            val arguments2 = arguments.map:
              case argument: TypeTree => TypeTree.of(using argument.tpe.asType)
              case argument           => argument

            TypeApply(transformTerm(fn)(sym), arguments2)

          case _ =>
            throw jl.Error()

    try Mapper.transformTerm(term)(Symbol.spliceOwner) catch case _: jl.Error => Unset


  def general[operand <: StaticAnnotation: Type, self: Type, plane: Type, limit: Type]
  :   Macro[self is Annotated by operand on plane under limit] =

    import quotes.reflect.{Annotated as _, *}

    val self = TypeRepr.of[self]
    val plane = TypeRepr.of[plane]
    val operand = TypeRepr.of[operand]
    val limit = TypeRepr.of[limit]
    val paramss = self.classSymbol.get.primaryConstructor.paramSymss
    val params = paramss.find(_.headOption.fold(false)(_.isTerm)).getOrElse(Nil)

    def matching(annotations: List[Term]): Expr[List[operand]] =
      Expr.ofList:
        annotations.map(_.asExpr).collect:
          case '{$annotation: `operand`} => rebuild(annotation.asTerm)

        . compact
        . map(_.asExprOf[operand])
        . reverse

    if limit =:= TypeRepr.of[Any] then
      val annotations = matching(self.typeSymbol.annotations)

      val params2 = params.per(plane.literal[String]): (params, field) =>
        params.filter(_.name == field)

      val fields =
        params2.flatMap: param =>
          if param.annotations.nil then Nil else
            List(param.name -> '{(${Expr(param.name)}.tt, ${matching(param.annotations)}.to(Set))})

        . to(Map)

      if fields.size == 1
      then
        val field: String = fields.head(0)
        val target: TypeRepr = ConstantType(StringConstant(field))

        (params.find(_.name == field).get.info.asType, target.asType).absolve match
          case ('[topic], '[type target <: Label; target]) =>
            ' {
                Annotated.AnnotatedField[operand, self, plane, limit, topic, target]
                  ( $annotations.to(Set), ${Expr.ofList(fields.values.to(List))}.to(Map) )
              }
      else
        ' {
            Annotated.AnnotatedFields[operand, self, plane, limit]
              ( $annotations.to(Set), ${Expr.ofList(fields.values.to(List))}.to(Map) )
          }

    else
      val subtypes = limit.typeSymbol.children.map: subtype =>
        '{(${Expr(subtype.name)}.tt, ${matching(subtype.annotations)}.to(Set))}

      ' {
          Annotated.AnnotatedSubtypes[operand, self, plane, limit]
            ( ${Expr.ofList(subtypes)}.to(Map) )
        }


  def dereferenceable[entity: Type, value: Type]: Macro[entity is Dereferenceable to value] =
    import quotes.reflect.*

    def namesList: Expr[List[Text]] = Expr.ofList:
      TypeRepr.of[entity]
      . typeSymbol
      . fieldMembers
      . filter(_.info <:< TypeRepr.of[value])
      . map: field => '{${Literal(StringConstant(field.name)).asExprOf[String]}.tt}

    def lambdaMap: Expr[List[(Text, entity => value)]] = Expr.ofList:
      TypeRepr.of[entity]
      . typeSymbol
      . fieldMembers
      . filter(_.info <:< TypeRepr.of[value])
      . map: field =>
          val name = '{${Literal(StringConstant(field.name)).asExprOf[String]}.tt}
          '{($name, (value: entity) => ${'value.asTerm.select(field).asExprOf[value]})}

    ' {
        new Dereferenceable:
          type Self = entity
          type Result = value
          private val lambdas: Map[Text, Self => Result] = ${lambdaMap}.toMap
          def names(entity: Self): List[Text] = ${namesList}
          def select(entity: entity, name: Text): Result = lambdas(name)(entity)
      }
