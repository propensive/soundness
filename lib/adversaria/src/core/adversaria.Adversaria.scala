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
┃    Soundness, version 0.53.0.                                                                    ┃
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

import anticipation.*
import denominative.*
import fulminate.*
import prepositional.*
import proscenium.*

import scala.quoted.*

object Adversaria:
  private def rebuild(using Quotes)(term: quotes.reflect.Term): quotes.reflect.Term =
    import quotes.reflect.*

    object Mapper extends TreeMap:
      override def transformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree =
        tree match
          case ident: TypeIdent =>
            val tree2 = TypeIdent(tree.symbol)
            tree2

      override def transformTerm(tree: Term)(owner: Symbol): Term =
        tree match
          case Ident(id) =>
            val tree2 = Ident(tree.symbol.termRef)
            tree2
          case Apply(fn, args) =>
            val tree2 = Apply(transformTerm(fn)(owner), transformTerms(args)(owner))
            tree2
          case Select(qualifier, name) =>
            val tree2 = Select(transformTerm(qualifier)(owner), tree.symbol)
            tree2
          case New(tpt) =>
            val tree2 = New(transformTypeTree(tpt)(owner))
            tree2
          case tree =>
            super.transformTerm(tree)(owner)

    Mapper.transformTerm(term)(Symbol.spliceOwner)


  def firstField[target <: Product: Type, annotation <: StaticAnnotation: Type]
  : Macro[CaseField[target, annotation]] =

      import quotes.reflect.*

      val targetType = TypeRepr.of[target]
      val fields = targetType.typeSymbol.caseFields

      val paramss = targetType.classSymbol.get.primaryConstructor.paramSymss
      val params = paramss.find(_.headOption.fold(false)( _.isTerm)).getOrElse(Nil)

      fields.zip(params).flatMap: (field, param) =>
        param.annotations.map(_.asExpr).collect:
          case '{$annotation: `annotation`} =>
            rebuild(annotation.asTerm).asExprOf[annotation]

        . map: annotation =>
            '{
                CaseField(Text(${Expr(field.name)}), (target: target) =>
                  ${'target.asTerm.select(field).asExpr}, $annotation)
            }
        . reverse
      . head


  def fields[target <: Product: Type, annotation <: StaticAnnotation: Type]
  : Macro[List[CaseField[target, annotation]]] =

      import quotes.reflect.*

      val targetType = TypeRepr.of[target]
      val fields = targetType.typeSymbol.caseFields

      val paramss = targetType.classSymbol.get.primaryConstructor.paramSymss
      val params = paramss.find(_.headOption.fold(false)( _.isTerm)).getOrElse(Nil)

      val elements: List[Expr[CaseField[target, annotation]]] =
        fields.zip(params).flatMap: (field, param) =>
          val name = Expr(field.name)
          param.annotations.map(_.asExpr).collect:
            case '{$annotation: `annotation`} =>
              rebuild(annotation.asTerm).asExprOf[annotation]

          . map: annotation =>
              '{CaseField(Text($name), (target: target) => ${'target.asTerm.select(field).asExpr},
                  $annotation)}

          . reverse

      Expr.ofList(elements)


  def dereferenceable[entity: Type, value: Type]: Macro[entity is Dereferenceable to value] =
    import quotes.reflect.*

    def namesList: Expr[List[Text]] = Expr.ofList:
      TypeRepr.of[entity]
      . typeSymbol
      . fieldMembers
      . filter(_.info <:< TypeRepr.of[value])
      . map: field =>
          '{  ${Literal(StringConstant(field.name)).asExprOf[String]}.tt }

    def lambdaMap: Expr[List[(Text, entity => value)]] = Expr.ofList:
      TypeRepr.of[entity]
      . typeSymbol
      . fieldMembers
      . filter(_.info <:< TypeRepr.of[value])
      . map: field =>
          val name = '{${Literal(StringConstant(field.name)).asExprOf[String]}.tt}
          '{  ($name, (value: entity) => ${'value.asTerm.select(field).asExprOf[value]}) }
    '{
        new Dereferenceable:
          type Self = entity
          type Result = value
          private val lambdas: Map[Text, Self => Result] = ${lambdaMap}.toMap
          def names(entity: Self): List[Text] = ${namesList}
          def select(entity: entity, name: Text): Result = lambdas(name)(entity)
    }

  def fieldAnnotations[target: Type](lambda: Expr[target => Any])
  : Macro[List[StaticAnnotation]] =

      import quotes.reflect.*

      val targetType = TypeRepr.of[target]
      val paramss = targetType.classSymbol.get.primaryConstructor.paramSymss
      val params = paramss.find(_.headOption.fold(false)( _.isTerm)).getOrElse(Nil)

      val field = lambda.asTerm match
        case Inlined(_, _, Block(List(DefDef(_, _, _, Some(Select(_, term)))), _)) =>
          params.find(_.name == term).getOrElse:
            panic(m"the member $term is not a case class field")

        case _ =>
          panic(m"the lambda must be a simple reference to a case class field")

      Expr.ofList:
        field.annotations.map(_.asExpr).collect:
          case '{ $annotation: StaticAnnotation } =>
            rebuild(annotation.asTerm).asExprOf[StaticAnnotation]


  def typeAnnotations[annotation <: StaticAnnotation: Type, target: Type]
  : Macro[Annotations[annotation, target]] =

      import quotes.reflect.*

      val targetType = TypeRepr.of[target]
      val annotations = targetType.typeSymbol.annotations.map(_.asExpr).collect:
        case '{$annotation: `annotation`} => annotation

      if annotations.nil then
        val typeName = TypeRepr.of[annotation].show
        panic(m"the type ${targetType.show} did not have the annotation $typeName")
      else '{Annotations[annotation, target](${Expr.ofList(annotations)}*)}
