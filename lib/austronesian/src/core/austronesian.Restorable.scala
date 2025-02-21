package austronesian

import scala.quoted.*

import anticipation.*
import hellenism.*
import wisteria.*

object Restorable extends Derivation[[Type] =>> Type is Restorable]:
  given Text is Restorable:
    def restore(value: Expr[Pojo])(using Quotes, Classloader): Expr[Text] =
      '{  if $value.isInstanceOf[String] then $value.asInstanceOf[Text]
          else throw new RuntimeException()  }

  given Int is Restorable:
    def restore(value: Expr[Pojo])(using Quotes, Classloader): Expr[Int] =
      '{  if $value.isInstanceOf[Int] then $value.asInstanceOf[Int]
          else throw new RuntimeException()  }

  inline def split[DerivationType: SumReflection]: DerivationType is Restorable =
    new Restorable:
      type Self = DerivationType

      def restore(value: Expr[Pojo])(using quotes: Quotes, classloader: Classloader): Expr[Self] =
        import quotes.reflect.*
        //given Type[DerivationType] = compiletime.summonInline[Type[DerivationType]]
        ???

  inline def join[DerivationType <: Product: ProductReflection]: DerivationType is Restorable =
    new Restorable:
      type Self = DerivationType

      def restore(value: Expr[Pojo])(using quotes: Quotes, classloader: Classloader): Expr[Self] =
        import quotes.reflect.*
        //given Type[DerivationType] = compiletime.summonInline[Type[DerivationType]]

        //val x: Symbol = TypeRepr.of[DerivationType].typeSymbol.primaryConstructor

        //println(x)
        ???

trait Restorable:
  type Self

  def restore(value: Expr[Pojo])(using Quotes, Classloader): Expr[Self]
