package polyvinyl

import rudiments.*

import scala.quoted.*
import scala.compiletime.*

trait SimpleSchema[F]:
  def fields: List[String]
  transparent inline def record(inline fn: String => F): SimpleRecord[F]
  
  final def build[F: Type](fn: Expr[String => F])(using Quotes): Expr[SimpleRecord[F]] =
    import quotes.*, quotes.reflect.*
    fields.foldLeft(TypeRepr.of[SimpleRecord[F]])(Refinement(_, _, TypeRepr.of[F])).asType match
      case '[typ] => '{new SimpleRecord[F]($fn(_)).asInstanceOf[typ & SimpleRecord[F]]}
      case _      => throw Impossible("the first case should always match")
  
class SimpleRecord[Field](fn: String => Field) extends Selectable:
  def selectDynamic(name: String): Field = fn(name)

class Record(fn: String => Any) extends Selectable:
  def selectDynamic(name: String): Any = fn(name)

trait Schema[E <: reflect.Enum]:
  def types: Map[String, E]
  transparent inline def record(inline fn: String => Any): Record
  type EnumType = E
  type Result[_ <: E]

  def build(fn: Expr[String => Any])(using Quotes, Type[E], Type[Result]): Expr[Record] =
    import quotes.*, quotes.reflect.*

    types.foldLeft(TypeRepr.of[Record]):
      case (acc, (key, etype)) =>
        val companion = Ref(TypeRepr.of[E].typeSymbol.companionModule)
        val sym = companion.symbol.declaredField(etype.toString)
        val returnType = Singleton(companion.select(sym)).tpe.asType match
          case '[typ] => TypeRepr.of[Result[typ & E]].simplified
          case _      => throw Impossible("the first case should always match")

        Refinement(acc, key, returnType)
    .asType match
      case '[typ] => '{new Record($fn(_)).asInstanceOf[typ & Record]}
      case _      => ???