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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gigantism.*
import hellenism.*
import prepositional.*
import proscenium.*
import rudiments.*
import wisteria.*

object protointernal:
  object EncodableDerivation extends Derivation[[entity] =>> entity is Encodable in Pojo]:
    inline def join[derivation <: Product: ProductReflection]
    :   derivation is Encodable in _root_.austronesian.internal.Pojo =

      fields(_): [field] => contextual.encoded(_)
      . asInstanceOf[Pojo]


    inline def split[derivation: SumReflection]: derivation is Encodable in Pojo =
      variant(_): [variant <: derivation] => value =>
        IArray.create[Pojo](2): array =>
          array(0) = label.s.asInstanceOf[Pojo]
          array(1) = value.encode

        . asInstanceOf[Pojo]

  object DecodableDerivation extends Derivable[Decodable in Pojo]:
    inline def join[derivation <: Product: ProductReflection]: derivation is Decodable in Pojo =
      case array: Array[Pojo @unchecked] =>
        build: [field] =>
          _.decoded(array(index))

      case other =>
        provide[Tactic[PojoError]](abort(PojoError()))

    inline def split[derivation: SumReflection]: derivation is Decodable in Pojo =
      case Array(label: String @unchecked, pojo: Pojo @unchecked) =>
        delegate(label): [variant <: derivation] => _.decoded(pojo)

      case other =>
        provide[Tactic[PojoError]](abort(PojoError()))

  def isolated[result: Type](classloader: Expr[Classloader], invoke: Expr[result]): Macro[result] =
    import quotes.reflect.*

    invoke.asTerm match
      case term => ()

    '{???}


  def proxy
    ( className:   Expr[Text],
      methodName:  Expr[String],
      arguments:   Expr[Seq[Any]],
      classloader: Expr[Classloader],
      singleton:   Expr[Boolean] )
  :   Macro[Any] =

    import quotes.reflect.*

    val arguments2: IArray[Expr[Pojo]] = arguments.absolve match
      case Varargs(arguments) => IArray.from(arguments).map:
        case '{$argument: argument} =>

          val encodable = Expr.summon[argument is Encodable in Pojo].getOrElse:
            halt(m"${TypeRepr.of[argument].show} is not encodable as a standard library parameter")

          '{$encodable.encoded($argument)}

        case _ =>
          panic(m"unmatched argument")

    if singleton.valueOrAbort then
      ' {
          val javaClass = Class.forName($className.s+"$", true, $classloader.java).nn
          val instance = javaClass.getField("MODULE$").nn.get(null).nn
          val method = javaClass.getMethod($methodName, classOf[Object]).nn
          method.invoke(instance, null)
        }
    else
      ' {
          val javaClass = Class.forName($className.s, true, $classloader.java).nn
          val method = javaClass.getMethod($methodName).nn
          method.invoke(null, null)
        }
