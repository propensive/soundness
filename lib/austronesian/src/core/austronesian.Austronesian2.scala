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
┃    Soundness, version 0.27.0.                                                                    ┃
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

import scala.compiletime.*
import scala.quoted.*

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import hellenism.*
import prepositional.*
import proscenium.*
import rudiments.*
import wisteria.*

given Realm = realm"austronesian"

object Austronesian2:
  object EncodableDerivation extends Derivation[[Type] =>> Type is Encodable in Stdlib]:

    inline def join[derivation <: Product: ProductReflection]
    :     derivation is Encodable in _root_.austronesian.Austronesian.Stdlib =

      fields(_):
        [field] => _.encode
      .asInstanceOf[Stdlib]

    inline def split[derivation: SumReflection]: derivation is Encodable in Stdlib =
      variant(_):
        [variant <: derivation] => value =>
          IArray.create[Stdlib](2): array =>
            array(0) = label.s.asInstanceOf[Stdlib]
            array(1) = value.encode

          . asInstanceOf[Stdlib]

  object DecodableDerivation extends Derivable[Decodable in Stdlib]:
    inline def join[derivation <: Product: ProductReflection]: derivation is Decodable in Stdlib =

      case array: Array[Stdlib] =>
        construct: [field] =>
          _.decoded(array(index))

      case other =>
        summonInline[Tactic[StdlibError]].give(abort(StdlibError()))

    inline def split[derivation: SumReflection]: derivation is Decodable in Stdlib =
      case Array(label: String @unchecked, stdlib: Stdlib @unchecked) =>
        delegate(label): [VariantType <: derivation] =>
          _.decoded(stdlib)

      case other =>
        summonInline[Tactic[StdlibError]].give(abort(StdlibError()))

  def isolated[ResultType: Type](classloader: Expr[Classloader], invoke: Expr[ResultType])
     (using Quotes)
  :     Expr[ResultType] =

    import quotes.reflect.*

    invoke.asTerm match
      case term => println(term)

    '{???}


  def proxy
     (className:   Expr[Text],
      methodName:  Expr[String],
      arguments:   Expr[Seq[Any]],
      classloader: Expr[Classloader],
      singleton:   Expr[Boolean])
     (using Quotes)
  :     Expr[Any] =

    import quotes.reflect.*

    val args: IArray[Expr[Stdlib]] = arguments.absolve match
      case Varargs(arguments) => IArray.from(arguments).map:
        case '{ $argument: argumentType } =>

          val encodable = Expr.summon[argumentType is Encodable in Stdlib].getOrElse:
            halt(m"${Type.of[argumentType]} is not encodable as a standard library parameter")

          '{$encodable.encoded($argument)}

    if singleton.valueOrAbort then
      '{  val javaClass = Class.forName($className.s+"$", true, $classloader.java).nn
          val instance = javaClass.getField("MODULE$").nn.get(null).nn
          val method = javaClass.getMethod($methodName, classOf[Object]).nn
          method.invoke(instance, null)  }
    else
      '{  val javaClass = Class.forName($className.s, true, $classloader.java).nn
          val method = javaClass.getMethod($methodName).nn
          method.invoke(null, null)  }
