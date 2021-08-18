package adversaria

import rudiments.*
import scala.quoted.*
import scala.annotation.StaticAnnotation
case class Annotations[A <: StaticAnnotation, T](annotations: A*)

object Annotations:
  inline given [A <: StaticAnnotation, T]: Annotations[A, T] = ${Macros.annotations[A, T]}

// abstract class AnnotatedParam[A <: StaticAnnotation, T](val name: String, val annotation: A):
//   type Param
//   def apply(value: T): Param

// object AnnotatedParam:
//   transparent inline given [A <: StaticAnnotation, T]: AnnotatedParam[A, T] =
//     ${Macros.annotatedParam[A, T]}

object Macros:
  def annotations[A <: StaticAnnotation: Type, T: Type](using Quotes): Expr[Annotations[A, T]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val annotations = tpe.typeSymbol.annotations.map(_.asExpr).collect { case '{ $a: A } => a }
    
    if annotations.isEmpty
    then report.throwError(str"adversaria: the type ${TypeRepr.of[T].show} did not have the "+
        str"annotation ${TypeRepr.of[A].show}")
    else '{ Annotations[A, T](${Expr.ofList(annotations)}*) }
  
  // def annotatedParam[A <: StaticAnnotation: Type, T: Type](using Quotes): Expr[AnnotatedParam[A, T]] =
  //   import quotes.reflect.*

  //   val tpe = TypeRepr.of[T]

  //   def findAnnotated(fields: List[Symbol])(using Quotes): List[Expr[AnnotatedParam[A, T]]] = fields match
  //     case Nil =>
  //       Nil
  //     case head :: tail =>
  //       head.tree.asExpr match
  //         case '{ $h: hType } =>
  //           head.annotations.map(_.asExpr).collect {
  //             case '{ $a: A } => '{
  //               new AnnotatedParam[A, T](${Expr(head.name)}, $a):
  //                 type Param = `hType`
  //                 def apply(value: T): Param = ???
  //             }
  //       }.to(List) ++ findAnnotated(tail)

  //   findAnnotated(tpe.typeSymbol.caseFields.collect {
  //     case sym: Symbol if sym.tree.isInstanceOf[ValDef] => sym
  //   }).headOption.getOrElse {
  //     report.throwError(str"could not find annotation ${TypeRepr.of[A].show}")
  //   }