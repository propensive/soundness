                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.41.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package austronesian

import scala.quoted.*
import scala.compiletime.*

import anticipation.*
import fulminate.*
import hellenism.*
import prepositional.*
import proscenium.*
import rudiments.*
import wisteria.*

object Restorable extends ProductDerivation[[entity] =>> entity is Restorable]:

  def apply[self](lambda: (Quotes, Classloader) ?=> Expr[Pojo] => Expr[self]): self is Restorable =
    new Restorable:
      type Self = self
      def restore(value: Expr[Pojo])(using Classloader) = lambda(value)

  given text: Text is Restorable:
    def restore(value: Expr[Pojo])(using Classloader): Macro[Text] =
      '{  if $value.isInstanceOf[String] then $value.asInstanceOf[Text]
          else throw new RuntimeException()  }

  given int: Int is Restorable:
    def restore(value: Expr[Pojo])(using Classloader): Macro[Int] =
      '{  if $value.isInstanceOf[Int] then $value.asInstanceOf[Int]
          else throw new RuntimeException()  }

  given long: Long is Restorable:
    def restore(value: Expr[Pojo])(using Classloader): Macro[Long] =
      '{  if $value.isInstanceOf[Long] then $value.asInstanceOf[Long]
          else throw new RuntimeException()  }

  given boolean: Boolean is Restorable:
    def restore(value: Expr[Pojo])(using Classloader): Macro[Boolean] =
      '{  if $value.isInstanceOf[Boolean] then $value.asInstanceOf[Boolean]
          else throw new RuntimeException()  }

  // inline def split[derivation: SumReflection]: derivation is Restorable =
  //   new Restorable:
  //     type Self = derivation

  //     def restore(value: Expr[Pojo])(using quotes: Quotes, classloader: Classloader): Expr[Self] =
  //       import quotes.reflect.*
  //       given Type[derivation] = infer[Type[derivation]]


  inline def join[derivation <: Product: ProductReflection]: derivation is Restorable =
    val cls = reflectClass[derivation]

    Restorable[derivation]: value =>
      val params = contexts:
        [field] => typeclass =>
          typeclass.restore('{$value.asInstanceOf[Array[Pojo]](${Expr[Int](index)})})

      '{  val cls0: Class[?] = Class.forName(${Expr(cls.getName.nn)}).nn
          val constructor = cls0.getDeclaredConstructors.nn(0).nn
          constructor.newInstance(${Varargs(params)}*).nn.asInstanceOf[derivation]  }

trait Restorable extends Typeclass:
  def restore(value: Expr[Pojo])(using Classloader): Macro[Self]
