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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package probably

import java.io as ji

import anticipation.*
import contingency.*
import gastronomy.*
import gossamer.*
import rudiments.*
import stratiform.*
import ulysses.*
import vacuous.*

// Writes a test run's `TestEvent`s as length-prefixed BinTEL frames to a host's
// `ji.OutputStream` — the single JDK-typed surface, because the host (a test-running tool such
// as fume) may call `stream` reflectively from ANOTHER CLASSLOADER WORLD, whose Soundness
// classes are not this one's. The first frame is a schema FINGERPRINT: both ends derive the
// BinTEL schema from their own `TestEvent` and compare fingerprints, so a version skew that
// changes the wire layout is detected before any event is decoded, rather than surfacing as a
// baffling decode failure.
object Streamer:
  // The derived BinTEL schema for `TestEvent`, computed once; every event encodes under it.
  private lazy val schema: Tels = Tels.tels[TestEvent](t"test-event")

  // A canonical structural rendering of the derived schema — name, document structure, and
  // select definitions with their variants, in declaration order — hashed with BLAKE3. This is
  // a fingerprint of the WIRE LAYOUT: two Soundness versions whose `TestEvent`s differ in any
  // field, order or case produce different fingerprints.
  lazy val fingerprint: Data =
    def renderType(kind: Tels.Type): Text = kind match
      case Tels.Struct(members, _) =>
        val rendered = members.to[List].map { (member: Tels.Member) => renderMember(member) }
        t"{${rendered.join(t";")}}"

      case Tels.Scalar(_, encoding, _) => t"scalar(${encoding.or(t"")})"
      case Tels.Flag                   => t"flag"
      case Tels.Reference(name)        => t"ref($name)"

    def renderMember(member: Tels.Member): Text = member match
      case Tels.Field(required, repeatable, keyword, fieldType, _, _, _) =>
        t"$keyword:${required.toString}:${repeatable.toString}:${renderType(fieldType)}"

      case Tels.SelectRef(required, repeatable, reference) =>
        t"select:$reference:${required.toString}:${repeatable.toString}"

      case Tels.Exclude(keyword) =>
        t"exclude:$keyword"

    def renderSelect(select: Tels.SelectDefinition): Text =
      val variants =
        select.variants.to[List].map: (variant: Tels.Variant) =>
          t"${variant.keyword}=${renderType(variant.variantType)}"

      t"${select.name}[${variants.join(t",")}]"

    val selects =
      schema.selects.to[List].map { (select: Tels.SelectDefinition) => renderSelect(select) }
    val rendering: Text = t"${schema.name}|${renderType(schema.document)}|${selects.join(t"|")}"

    Blake3.hashOf(rendering.sysData, 32)

  private def encode(event: TestEvent): Data = unsafely(event.tel.bintel(schema))

  // One frame: a 4-byte big-endian length prefix, then the payload, flushed so the host's
  // chunk stream sees it promptly.
  private def frame(output: ji.OutputStream, payload: Data): Unit =
    val length: Int = payload.length

    val header: Data =
      Array[Byte]
        ( ((length >> 24) & 0xff).toByte,
          ((length >> 16) & 0xff).toByte,
          ((length >> 8) & 0xff).toByte,
          (length & 0xff).toByte )

    output.write(header.mutable(using Unsafe))
    output.write(payload.mutable(using Unsafe))
    output.flush()

  // Runs the named suite (a `probably.Suite` object, loaded from THIS classloader — the
  // suite's own world, where `Suite` is nameable) with the given selection arguments
  // (newline-separated, as `Suite#invoke`), writing the fingerprint frame and then one frame
  // per event. Returns the suite's exit status (0 = passed, 1 = failures, 2 = the suite or the
  // machinery threw). Erases to `stream(String, String, OutputStream): int`, so a host in
  // another classloader world can call it through a structural type with JDK types alone.
  def stream(suite: Text, arguments: Text, output: ji.OutputStream): Int =
    val writer: Mutex = Mutex()
    def send(payload: Data): Unit = writer(frame(output, payload))

    val status: Int =
      safely:
        send(fingerprint)

        val moduleClass = Class.forName(suite.s + "$", true, getClass.getClassLoader).nn
        val instance = moduleClass.getField("MODULE$").nn.get(null).nn

        instance.absolve match
          case suite: Suite =>
            suite.invoke(arguments, { (event: TestEvent) => send(encode(event)) })

      . or(2)

    safely(output.close())
    status
