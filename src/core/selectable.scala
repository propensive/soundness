package polyvinyl

import scala.quoted.*
import scala.compiletime.*

trait Schema[F]:
  def fields: List[String]
  transparent inline def record(inline fn: String => F): Record[F]
  
  final def build[F: Type](fn: Expr[String => F])(using Quotes): Expr[Record[F]] =
    import quotes.*, quotes.reflect.*
    fields.foldLeft(TypeRepr.of[Record[F]])(Refinement(_, _, TypeRepr.of[F])).asType match
      case '[typ] => '{new Record[F]($fn(_)).asInstanceOf[typ & Record[F]]}
      case _      => ???
  
class Record[Field](fn: String => Field) extends Selectable:
  def selectDynamic(name: String): Field = fn(name)

class TypedRecord(fn: String => Any) extends Selectable:
  def selectDynamic(name: String): Any = fn(name)

trait TypedSchema[E <: reflect.Enum]:
  def types: Map[String, E]
  transparent inline def record(inline fn: String => Any): TypedRecord
  type EnumType = E
  type Result[E1 <: E]

  def build(fn: Expr[String => Any])(using Quotes, Type[E], Type[Result]): Expr[TypedRecord] =
    import quotes.*, quotes.reflect.*

    types.foldLeft(TypeRepr.of[TypedRecord]):
      case (acc, (key, etype)) =>
        val companion = Ref(TypeRepr.of[E].typeSymbol.companionModule)
        val sym = companion.symbol.declaredField(etype.toString)
        val returnType = Singleton(companion.select(sym)).tpe.asType match
          case '[typ] => TypeRepr.of[Result[typ & E]].simplified

        Refinement(acc, key, returnType)
    .asType match
      case '[typ] => '{new TypedRecord($fn(_)).asInstanceOf[typ & TypedRecord]}
      case _      => ???