package wisteria

import scala.deriving.*
import scala.compiletime.*

trait SingletonFactory[T] {
  def create: T
}

inline given singletonFactory[T <: Product](using reflection: ProductReflection[T]): SingletonFactory[T] = 
  compiletime.summonFrom:
    case given (reflection.MirroredMonoType <:< Singleton) => 
      new SingletonFactory[T]:
        override def create: T = reflection.fromProduct(EmptyTuple)
    case _ =>
      inline val typeName: String = erasedValue[reflection.MirroredLabel]
      error("Cannot derive sumSimpleVariant for '"+typeName+"'")