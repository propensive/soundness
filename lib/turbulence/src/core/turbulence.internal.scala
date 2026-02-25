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
package turbulence

import scala.quoted.*

import anticipation.*
import denominative.*
import fulminate.*
import hieroglyph.*
import prepositional.*
import proscenium.*
import stenography.*
import vacuous.*

object internal:
  import stenography.internal.name

  def read[source: Type, result: Type](source: Expr[source]): Macro[result] =
    lazy val streamableData: Optional[Expr[source is Streamable by Data]] =
      Expr.summon[source is Streamable by Data].optional

    lazy val streamableText: Optional[Expr[source is Streamable by Text]] =
      Expr.summon[source is Streamable by Text].optional

    lazy val aggregableData: Optional[Expr[result is Aggregable by Data]] =
      Expr.summon[result is Aggregable by Data].optional

    lazy val aggregableText: Optional[Expr[result is Aggregable by Text]] =
      Expr.summon[result is Aggregable by Text].optional

    lazy val decoder: Optional[Expr[CharDecoder]] = Expr.summon[CharDecoder].optional
    lazy val encoder: Optional[Expr[CharEncoder]] = Expr.summon[CharEncoder].optional

    lazy val streamables = List(streamableData, streamableText).compact
    lazy val aggregables = List(aggregableData, aggregableText).compact

    streamableData.let: streamable =>
      aggregableData.let: aggregable =>
        '{$aggregable.aggregate($streamable.stream($source))}

    . or:
        streamableText.let: streamable =>
          aggregableText.let: aggregable =>
            '{$aggregable.aggregate($streamable.stream($source))}

    . or:
        streamableData.let: streamable =>
          aggregableText.let: aggregable =>
            decoder.let: decoder =>
              '{$aggregable.aggregate($decoder.decoded($streamable.stream($source)))}

    . or:
        streamableText.let: streamable =>
          aggregableData.let: aggregable =>
            encoder.let: encoder =>
              '{$aggregable.aggregate($encoder.encoded($streamable.stream($source)))}

    . or:

        val reason =
          if streamables.nil && aggregables.nil
          then
            m"""
              no ${name[source is Streamable]} or ${name[result is Aggregable]} instance exists in
              context
            """
          else if streamableData.present && aggregableText.present
          then
            m"""
              although ${name[source is Streamable by Data]} and
              ${name[result is Aggregable by Text]} instances exist in context, a
              ${name[CharDecoder]} instance is required to convert between them
            """
          else if streamableText.present && aggregableData.present
          then
            m"""
              although ${name[source is Streamable by Text]} and
              ${name[result is Aggregable by Data]} instances exist in context, a
              ${name[CharEncoder]} instance is required to convert between them
            """
          else if streamables.length == 2
          then
            m"""
              although ${name[source is Streamable by Data]} and
              ${name[source is Streamable by Text]} instances exist in context, no
              ${name[result is Aggregable]} was found
            """
          else if streamableData.present
          then
            m"""
              although a ${name[source is Streamable by Data]} instance exists in context, no
              ${name[result is Aggregable]} was found
            """
          else if streamableText.present
          then
            m"""
              although a ${name[source is Streamable by Text]} instance exists in context, no
              ${name[result is Aggregable]} was found
            """
          else if aggregables.length == 2
          then
            m"""
              although ${name[result is Aggregable by Data]} and
              ${name[result is Aggregable by Text]} instances exist in context, no
              ${name[source is Streamable]} was found
            """
          else if aggregableData.present
          then
            m"""
              although a ${name[result is Aggregable by Data]} instance exists in context, no
              ${name[source is Streamable]} was found
            """
          else
            m"""
              although a ${name[result is Aggregable by Text]} instance exists in context, no
              ${name[source is Streamable]} was found
            """

        halt(m"unable to read ${name[source]} as ${name[result]}: "+reason)

  def stream[source: Type, operand: Type](source: Expr[source]): Macro[Stream[operand]] =
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
            case '{$stream: Stream[`operand`]} => stream

        . or:
            halt(m"can not stream ${name[source]} as ${name[Text]} without a ${name[CharDecoder]}")

        else if bytes && streamableText.present then encoder.let: encoder =>
          '{$encoder.encoded(${streamableText.vouch}.stream($source))}.absolve match
            case '{$stream: Stream[`operand`]} => stream

        . or:
            halt(m"can not stream ${name[source]} as ${name[Data]} without a ${name[CharEncoder]}")

        else halt(m"no ${name[source is Streamable by operand]} (or $otherName) was found")
