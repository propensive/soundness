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
package adversaria

import anticipation.*
import fulminate.*
import proscenium.*

import scala.quoted.*

object Adversaria:
  def firstField[target <: Product: Type, annotation <: StaticAnnotation: Type]
  : Macro[CaseField[target, annotation]] =

      import quotes.reflect.*

      val targetType = TypeRepr.of[target]
      val fields = targetType.typeSymbol.caseFields

      fields.flatMap: field =>
        field.annotations.map(_.asExpr).collect:
          case '{$annotation: `annotation`} => annotation

        . map: annotation =>
            '{CaseField(Text(${Expr(field.name)}), (target: target) =>
                ${'target.asTerm.select(field).asExpr}, $annotation)}
        . reverse
      . head


  def fields[target <: Product: Type, annotation <: StaticAnnotation: Type]
  : Macro[List[CaseField[target, annotation]]] =

      import quotes.reflect.*

      val targetType = TypeRepr.of[target]
      val fields = targetType.typeSymbol.caseFields

      val elements: List[Expr[CaseField[target, annotation]]] = fields.flatMap: field =>
        val name = Expr(field.name)
        field.annotations.map(_.asExpr).collect:
          case '{$annotation: `annotation`} => annotation

        . map: annotation =>
            '{CaseField(Text($name), (target: target) => ${'target.asTerm.select(field).asExpr},
                $annotation)}

        . reverse

      Expr.ofList(elements)


  def fieldAnnotations[target: Type](lambda: Expr[target => Any])
  : Macro[List[StaticAnnotation]] =

      import quotes.reflect.*

      val targetType = TypeRepr.of[target]

      val field = lambda.asTerm match
        case Inlined(_, _, Block(List(DefDef(_, _, _, Some(Select(_, term)))), _)) =>
          targetType.typeSymbol.caseFields.find(_.name == term).getOrElse:
            panic(m"the member $term is not a case class field")

        case _ =>
          panic(m"the lambda must be a simple reference to a case class field")

      Expr.ofList:
        field.annotations.map(_.asExpr).collect:
          case '{ $annotation: StaticAnnotation } => annotation


  def typeAnnotations[annotation <: StaticAnnotation: Type, target: Type]
  : Macro[Annotations[annotation, target]] =

      import quotes.reflect.*

      val targetType = TypeRepr.of[target]
      val annotations = targetType.typeSymbol.annotations.map(_.asExpr).collect:
        case '{$annotation: `annotation`} => annotation

      if annotations.isEmpty
      then
        val typeName = TypeRepr.of[annotation].show
        panic(m"the type $targetType did not have the annotation $typeName")
      else '{Annotations[annotation, target](${Expr.ofList(annotations)}*)}
