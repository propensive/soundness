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
┃    Soundness, version 0.45.0.                                                                    ┃
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
import hieroglyph.*
import prepositional.*
import proscenium.*
import stenography.*
import vacuous.*
import scala.languageFeature.existentials

private given Realm = Realm("turbulence")

object Turbulence:
  import Stenography.name

  def read[source: Type, result: Type](source: Expr[source]): Macro[result] =
    import quotes.reflect.*
    lazy val streamableBytes: Optional[Expr[source is Streamable by Bytes]] =
      Expr.summon[source is Streamable by Bytes].optional

    lazy val streamableText: Optional[Expr[source is Streamable by Text]] =
      Expr.summon[source is Streamable by Text].optional

    lazy val aggregableBytes: Optional[Expr[result is Aggregable by Bytes]] =
      Expr.summon[result is Aggregable by Bytes].optional

    lazy val aggregableText: Optional[Expr[result is Aggregable by Text]] =
      Expr.summon[result is Aggregable by Text].optional

    lazy val decoder: Optional[Expr[CharDecoder]] = Expr.summon[CharDecoder].optional
    lazy val encoder: Optional[Expr[CharEncoder]] = Expr.summon[CharEncoder].optional

    lazy val streamables = List(streamableBytes, streamableText).compact
    lazy val aggregables = List(aggregableBytes, aggregableText).compact

    streamableBytes.let: streamable =>
      aggregableBytes.let: aggregable =>
        '{$aggregable.aggregate($streamable.stream($source))}

    . or:
        streamableText.let: streamable =>
          aggregableText.let: aggregable =>
            '{$aggregable.aggregate($streamable.stream($source))}

    . or:
        streamableBytes.let: streamable =>
          aggregableText.let: aggregable =>
            decoder.let: decoder =>
              '{$aggregable.aggregate($decoder.decoded($streamable.stream($source)))}
    . or:
        streamableText.let: streamable =>
          aggregableBytes.let: aggregable =>
            encoder.let: encoder =>
              '{$aggregable.aggregate($encoder.encoded($streamable.stream($source)))}

    . or:

        val reason =
          if streamables.isEmpty && aggregables.isEmpty
          then m"""no ${name[source is Streamable]} or ${name[result is Aggregable]} instance exists
                   in context"""
          else if streamableBytes.present && aggregableText.present
          then m"""although ${name[source is Streamable by Bytes]} and
                  ${name[result is Aggregable by Text]} instances exist in context, a
                  ${name[CharDecoder]} instance is required to convert between them"""
          else if streamableText.present && aggregableBytes.present
          then m"""although ${name[source is Streamable by Text]} and
                  ${name[result is Aggregable by Bytes]} instances exist in context, a
                  ${name[CharEncoder]} instance is required to convert between them"""
          else if streamables.length == 2
          then m"""although ${name[source is Streamable by Bytes]} and
                   ${name[source is Streamable by Text]} instances exist in context, no
                   ${name[result is Aggregable]} was found"""
          else if streamableBytes.present
          then m"""although a ${name[source is Streamable by Bytes]} instance exists in context, no
                   ${name[result is Aggregable]} was found"""
          else if streamableText.present
          then m"""although a ${name[source is Streamable by Text]} instance exists in context, no
                   ${name[result is Aggregable]} was found"""
          else if aggregables.length == 2
          then m"""although ${name[result is Aggregable by Bytes]} and
                   ${name[result is Aggregable by Text]} instances exist in context, no
                   ${name[source is Streamable]} was found"""
          else if aggregableBytes.present
          then m"""although a ${name[result is Aggregable by Bytes]} instance exists in context, no
                  ${name[source is Streamable]} was found"""
          else m"""although a ${name[result is Aggregable by Text]} instance exists in context, no
                  ${name[source is Streamable]} was found"""

        halt(m"unable to read ${name[source]} as ${name[result]}: "+reason)
