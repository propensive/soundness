/*
    Adversaria, version 0.4.0. Copyright 2019-23 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package adversaria

import rudiments.*
import scala.quoted.*
import scala.annotation.StaticAnnotation as Ann

case class Annotations[A <: Ann, T](annotations: A*)

object Annotations:
  inline given [A <: Ann, T]: Annotations[A, T] = ${AdversariaMacros.typeAnnotations[A, T]}

  transparent inline def field[T](inline fn: T => Any): List[Ann] =
    ${AdversariaMacros.fieldAnnotations[T]('fn)}

  transparent inline def fields[T <: Product, A <: Ann]: List[CaseField[T, A]] =
    ${AdversariaMacros.fields[T, A]}

  transparent inline def firstField[T <: Product, A <: Ann]: CaseField[T, A] =
    ${AdversariaMacros.firstField[T, A]}

object CaseField:
  def apply[T <: Product, A <: Ann, F](name: Text, access: T => F, ann: A)
      : CaseField[T, A] { type FieldType = F } =
    new CaseField[T, A](name):
      type FieldType = F
      def apply(value: T) = access(value)
      def annotation: A = ann

  transparent inline given [T <: Product, A <: Ann]: CaseField[T, A] = Annotations.firstField[T, A]

trait CaseField[T <: Product, A <: Ann](val name: Text):
  type FieldType
  def apply(value: T): FieldType
  def annotation: A

object AdversariaMacros:
  def firstField[T <: Product: Type, A <: Ann: Type](using Quotes): Expr[CaseField[T, A]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val fields = tpe.typeSymbol.caseFields
    
    fields.flatMap: fld =>
      fld.annotations.map(_.asExpr).collect { case '{ $ann: A } => ann }.map: ann =>
        '{ CaseField(Text(${Expr(fld.name)}), (t: T) => ${'t.asTerm.select(fld).asExpr}, $ann) }
      .reverse
    .head

  def fields[T <: Product: Type, A <: Ann: Type](using Quotes): Expr[List[CaseField[T, A]]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val fields = tpe.typeSymbol.caseFields
    
    val elements: List[Expr[CaseField[T, A]]] = fields.flatMap: fld =>
      val name = Expr(fld.name)
      fld.annotations.map(_.asExpr).collect { case '{ $ann: A } => ann }.map: ann =>
        '{ CaseField(Text($name), (t: T) => ${'t.asTerm.select(fld).asExpr}, $ann) }
      .reverse

    Expr.ofList(elements)

  def fieldAnnotations[T: Type](fn: Expr[T => Any])(using Quotes): Expr[List[Ann]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]

    val field = fn.asTerm match
      case Inlined(_, _, Block(List(DefDef(_, _, _, Some(Select(_, term)))), _)) =>
        tpe.typeSymbol.caseFields.find(_.name == term).getOrElse:
          report.errorAndAbort(s"adversaria: the member $term is not a case class field")
      
      case _ =>
        report.errorAndAbort:
          """adversaria: the lambda must be a simple reference to a case class field"""

    Expr.ofList(field.annotations.map(_.asExpr).collect { case '{ $ann: Ann } => ann })

  def typeAnnotations[A <: Ann: Type, T: Type](using Quotes): Expr[Annotations[A, T]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val annotations = tpe.typeSymbol.annotations.map(_.asExpr).collect { case '{ $a: A } => a }
    
    if annotations.isEmpty
    then report.errorAndAbort(
        s"""adversaria: the type ${tpe.show} did not have the annotation ${TypeRepr.of[A].show}""")
    else '{ Annotations[A, T](${Expr.ofList(annotations)}*) }
