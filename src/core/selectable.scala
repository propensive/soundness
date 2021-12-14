package polyvinyl

import scala.quoted.*

trait Schema[F]:
  def fields: List[String]
  
  final def build[F: Type, S <: Singleton & Schema[F]: Type](fn: Expr[String => F])(using Quotes): Expr[Record[F]] =
    import quotes.*, quotes.reflect.*
    val typ = TypeRepr.of[Record[F]]
    fields.foldLeft(typ)(Refinement(_, _, TypeRepr.of[F])).asType match
      case '[typ] => '{new Record[F]($fn(_)).asInstanceOf[typ & Record[F]]}
      case _      => ???
  
  transparent inline def record(inline fn: String => Int): Record[Int]

class Record[Field](fn: String => Field) extends Selectable:
  def selectDynamic(name: String): Field = fn(name)
