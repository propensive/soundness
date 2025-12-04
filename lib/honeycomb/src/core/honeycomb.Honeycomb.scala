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
┃    Soundness, version 0.46.0.                                                                    ┃
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
package honeycomb

import anticipation.*
import fulminate.*
import prepositional.*
import proscenium.*
import rudiments.*
import stenography.*
import vacuous.*

import scala.quoted.*

object Honeycomb:
  private given realm: Realm = realm"honeycomb"

  def attributes[result: Type, thisType <: Tag to result: Type]
       (tag: Expr[Tag], attributes0: Expr[Seq[(String, Any)]])
  : Macro[result] =
      import quotes.reflect.*

      val Varargs(args) = attributes0

      val attributes: Seq[Expr[Optional[(Text, Optional[Text])]]] =
        Type.of[thisType] match
          case '[ type topic <: Label; Tag { type Topic = topic } ] => args.map:
            case '{ ($key, $value: value) } =>
              TypeRepr.of[topic].literal[String].let: topic =>
                key.asTerm match
                  case Literal(StringConstant(key)) =>
                    ConstantType(StringConstant(key)).asType match
                      case '[type key <: Label; key] =>
                        Expr.summon[key is Attribute in Html5.type on (? >: topic)]
                        . orElse(Expr.summon[key is Attribute in Html5.type]) match
                          case Some('{ type result;
                                       $expr: Attribute { type Topic = result } }) =>
                            Expr.summon[value is Attributive to result] match
                              case Some('{ $converter: Attributive }) =>
                                '{$converter.attribute(${Expr(key)}, $value)}

                              case None =>
                                halt(m"there is not converter for ${TypeRepr.of[result].show} attributes")

                          case _ =>
                            halt(m"attribute $key cannot be used on tag <$topic>")

                  case _ =>
                    halt(m"unexpectedly unable to determine attribute key")
              . or(halt(m"unexpected type"))


      '{$tag.node(${Expr.ofList(attributes)})}.asExprOf[result]
