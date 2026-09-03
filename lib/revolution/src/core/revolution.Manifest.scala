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
package revolution

import contingency.*
import gossamer.*

import java.io as ji
import java.util.jar as juj

import anticipation.*
import denominative.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*
import zephyrine.Credit
import vacuous.*

object Manifest:
  protected def parse[streamable: Streamable by Data over Credit](source: streamable): Manifest =
    val java = juj.Manifest(source.source[Data].inputStream)

    Manifest:
      (java.getMainAttributes.nn: _root_.java.util.Map[Object, Object]).to[List].map: (key, value) =>
        (key.toString.tt, value.toString.tt)

      . to[Map]

  given streamable: Manifest is Streamable by Data over Credit = manifest =>
    zephyrine.Stream(manifest.serialize)
  given aggregable: Manifest is Aggregable by Data = parse(_)

  def apply(entries: Manifest.Entry*): Manifest = Manifest:
    Map.from:
      entries.map: entry =>
        (entry.key, entry.value)

  given addable: Manifest is Addable by Manifest.Entry to Manifest = Addable: (manifest, entry) =>
    Manifest(manifest.entries.define(entry.key, entry.value))

  given subtractable: [key <: Label, attribute <: Manifest.Attribute[key]]
  =>  Manifest is Subtractable by attribute to Manifest =

    Subtractable: (manifest, attribute) => Manifest(manifest.entries.omit(attribute.key))

  // ManifestAttribute → Manifest.Attribute
  abstract class Attribute[label <: Label: ValueOf]():
    val key: Text = valueOf[label].tt

    def parse(value: Text)(using decoder: label is DecodableManifest): decoder.Topic =
      decoder.decoded(value)

    def apply(using encoder: label is EncodableManifest)(value: encoder.Topic): Manifest.Entry =
      Manifest.Entry(key, encoder.encode(value))

  // ManifestEntry → Manifest.Entry
  case class Entry(key: Text, value: Text)

case class Manifest(entries: Map[Text, Text]):
  def apply[key <: Label: DecodableManifest](attribute: Manifest.Attribute[key])
  :   Optional[key.Topic] =

    entries(attribute.key).let(key.decoded(_))


  def serialize: Data =
    val manifest = juj.Manifest()

    entries.each: (key, value) =>
      manifest.getMainAttributes.nn.putValue(key.s, value.s)

    val out = ji.ByteArrayOutputStream()
    manifest.write(out)
    Array.unsafeFrozen(out.toByteArray().nn)
