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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package turbulence

import scala.quoted.*

import anticipation.*
import fulminate.*
import gigantism.*
import hieroglyph.*
import prepositional.*
import vacuous.*

object internal:
  import stenography.internal.name

  def lazyList[source: Type, operand: Type](source: Expr[source]): Macro[LazyList[operand]] =
    import quotes.reflect.*

    val bytes = TypeRepr.of[operand] =:= TypeRepr.of[Data]
    val text = TypeRepr.of[operand] =:= TypeRepr.of[Text]

    lazy val streamableData: Optional[Expr[source is Streamable by Data]] =
      Expr.summon[source is Streamable by Data].optional

    lazy val streamable: Optional[Expr[source is Streamable by operand]] =
      Expr.summon[source is Streamable by operand].optional

    lazy val streamableText: Optional[Expr[source is Streamable by Text]] =
      Expr.summon[source is Streamable by Text].optional

    lazy val decoder: Optional[Expr[CharDecoder]] = Expr.summon[CharDecoder].optional
    lazy val encoder: Optional[Expr[CharEncoder]] = Expr.summon[CharEncoder].optional

    val otherName =
      if bytes then name[source is Streamable by Text] else name[source is Streamable by Data]

    Expr.summon[source is Streamable by operand].optional.let: streamable =>
      '{$streamable.stream($source)}

    . or:
        if text && streamableData.present then decoder.let: decoder =>
          '{$decoder.decoded(${streamableData.vouch}.stream($source))}.absolve match
            case '{$stream: LazyList[`operand`]} => stream

        . or:
            halt(m"can not stream ${name[source]} as ${name[Text]} without a ${name[CharDecoder]}")

        else if bytes && streamableText.present then encoder.let: encoder =>
          '{$encoder.encoded(${streamableText.vouch}.stream($source))}.absolve match
            case '{$stream: LazyList[`operand`]} => stream

        . or:
            halt(m"can not stream ${name[source]} as ${name[Data]} without a ${name[CharEncoder]}")

        else halt(m"no ${name[source is Streamable by operand]} (or $otherName) was found")
