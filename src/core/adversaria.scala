package adversaria

import rudiments.*
import gossamer.*
import scala.quoted.*
import scala.annotation.StaticAnnotation as Ann

case class Annotations[A <: Ann, T](annotations: A*)

object Annotations:
  inline given [A <: Ann, T]: Annotations[A, T] = ${Macros.typeAnnotations[A, T]}

  transparent inline def field[T](inline fn: T => Any): List[Ann] =
    ${Macros.fieldAnnotations[T]('fn)}

  transparent inline def fields[T <: Product, A <: Ann]: List[CaseField[T, A]] =
    ${Macros.fields[T, A]}

  transparent inline def firstField[T <: Product, A <: Ann]: CaseField[T, A] =
    ${Macros.firstField[T, A]}

object CaseField:
  def apply[T <: Product, A <: Ann, F](name: String, access: T => F, ann: A)
      : CaseField[T, A] { type FieldType = F } =
    new CaseField[T, A](name):
      type FieldType = F
      def apply(value: T) = access(value)
      def annotation: A = ann

trait CaseField[T <: Product, A <: Ann](val name: String):
  type FieldType
  def apply(value: T): FieldType
  def annotation: A

object Macros:
  def firstField[T <: Product: Type, A <: Ann: Type](using Quotes): Expr[CaseField[T, A]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val fields = tpe.typeSymbol.caseFields
    
    fields.flatMap { fld =>
      val idx = Expr(fields.indexOf(fld))
      val name = Expr(fld.name)
      fld.annotations.map(_.asExpr).collect { case '{ $ann: A } => ann }.map { ann =>
        '{ CaseField($name, (t: T) => ${'t.asTerm.select(fld).asExpr}, $ann) }
      }.reverse
    }.head

  def fields[T <: Product: Type, A <: Ann: Type](using Quotes): Expr[List[CaseField[T, A]]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val fields = tpe.typeSymbol.caseFields
    
    val elements: List[Expr[CaseField[T, A]]] = fields.flatMap { fld =>
      val name = Expr(fld.name)
      fld.annotations.map(_.asExpr).collect { case '{ $ann: A } => ann }.map { ann =>
        '{ CaseField($name, (t: T) => ${'t.asTerm.select(fld).asExpr}, $ann) }
      }.reverse
    }

    Expr.ofList(elements)

  def fieldAnnotations[T: Type](fn: Expr[T => Any])(using Quotes): Expr[List[Ann]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]

    val field = fn.asTerm match
      case Inlined(_, _, Block(List(DefDef(_, _, _, Some(Select(_, term)))), _)) =>
        tpe.typeSymbol.caseFields.find(_.name == term).getOrElse {
          report.throwError(txt"adversaria: the member $term is not a case class field".s)
        }
      
      case _ =>
        report.throwError(txt"""adversaria: the lambda must be a simple reference to a case class
            field""".s)

    Expr.ofList(field.annotations.map(_.asExpr).collect {
      case '{ $ann: Ann } => ann
    })

  def typeAnnotations[A <: Ann: Type, T: Type](using Quotes): Expr[Annotations[A, T]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val annotations = tpe.typeSymbol.annotations.map(_.asExpr).collect { case '{ $a: A } => a }
    
    if annotations.isEmpty
    then report.throwError(txt"""adversaria: the type ${TypeRepr.of[T].show} did not have the
        annotation ${TypeRepr.of[A].show}""".s)
    else '{ Annotations[A, T](${Expr.ofList(annotations)}*) }