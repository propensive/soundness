package entry

import vacuous.*
import rudiments.*
import wisteria.*

object Main extends App {
  val t: Optional[String] = "hello"
  println(2.unit)
  println(t)
  println("hello world!")

  trait Show[T] {
    def show(value: T): String
  }

  extension[T: Show](value: T) {
    def show: String = summon[Show[T]].show(value)
  }

  object Show extends Derivation[Show] {

    inline def join[DerivationType <: Product: ProductReflection]: Show[DerivationType] = value =>
      typeName.s

    inline def split[DerivationType: SumReflection]: Show[DerivationType] = value =>
      inline if choice then
        variant(value): 
          [VariantType <: DerivationType] =>
            variant => typeName.s+"."+variant.show
      else
        compiletime.error("cannot derive Show for adt")
  }

  enum Simple:
    case First
    case Second
    case Third

  println("isSimpleSum")
  val showForSimple = summon[Show[Simple]]
  println(showForSimple.show(Simple.Second))
}
