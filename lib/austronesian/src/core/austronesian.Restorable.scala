package austronesian

import scala.quoted.*
import scala.compiletime.*

import anticipation.*
import fulminate.*
import hellenism.*
import rudiments.*
import wisteria.*

object Restorable extends ProductDerivation[[entity] =>> entity is Restorable]:

  def apply[self](lambda: (Quotes, Classloader) ?=> Expr[Pojo] => Expr[self]): self is Restorable =
    new Restorable:
      type Self = self
      def restore(value: Expr[Pojo])(using Quotes, Classloader) = lambda(value)

  given text: Text is Restorable:
    def restore(value: Expr[Pojo])(using Quotes, Classloader): Expr[Text] =
      '{  if $value.isInstanceOf[String] then $value.asInstanceOf[Text]
          else throw new RuntimeException()  }

  given int: Int is Restorable:
    def restore(value: Expr[Pojo])(using Quotes, Classloader): Expr[Int] =
      '{  if $value.isInstanceOf[Int] then $value.asInstanceOf[Int]
          else throw new RuntimeException()  }

  given long: Long is Restorable:
    def restore(value: Expr[Pojo])(using Quotes, Classloader): Expr[Long] =
      '{  if $value.isInstanceOf[Long] then $value.asInstanceOf[Long]
          else throw new RuntimeException()  }

  given boolean: Boolean is Restorable:
    def restore(value: Expr[Pojo])(using Quotes, Classloader): Expr[Boolean] =
      '{  if $value.isInstanceOf[Boolean] then $value.asInstanceOf[Boolean]
          else throw new RuntimeException()  }

  // inline def split[derivation: SumReflection]: derivation is Restorable =
  //   new Restorable:
  //     type Self = derivation

  //     def restore(value: Expr[Pojo])(using quotes: Quotes, classloader: Classloader): Expr[Self] =
  //       import quotes.reflect.*
  //       given Type[derivation] = compiletime.summonInline[Type[derivation]]


  inline def join[derivation <: Product: ProductReflection]: derivation is Restorable =
    val cls = reflectClass[derivation]

    Restorable[derivation]: value =>
      val params = contexts:
        [field] => typeclass =>
          typeclass.restore('{$value.asInstanceOf[Array[Pojo]](${Expr[Int](index)})})

      '{  val cls0: Class[?] = Class.forName(${Expr(cls.getName.nn)}).nn
          val constructor = cls0.getDeclaredConstructors.nn(0).nn
          constructor.newInstance(${Varargs(params)}*).nn.asInstanceOf[derivation]  }

trait Restorable:
  type Self

  def restore(value: Expr[Pojo])(using Quotes, Classloader): Expr[Self]
