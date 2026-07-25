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

import proscenium.compat.*

import java.io as ji
import java.util.jar as juj

import anticipation.*
import denominative.*
import prepositional.*
import rudiments.*
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

      . pipe(l => Map.from(l.stdlib))

  given streamable: Manifest is Streamable by Data over Credit = manifest =>
    zephyrine.Stream(manifest.serialize)
  given aggregable: Manifest is Aggregable by Data = parse(_)

  def apply(entries: ManifestEntry*): Manifest = Manifest:
    entries.map: entry =>
      (entry.key, entry.value)

    . pipe(Map.from(_))

  given addable: Manifest is Addable by ManifestEntry to Manifest = Addable: (manifest, entry) =>
    Manifest(manifest.entries.updated(entry.key, entry.value))


  given subtractable: [key <: Label, attribute <: ManifestAttribute[key]]
  =>  Manifest is Subtractable by attribute to Manifest =

    Subtractable: (manifest, attribute) => Manifest(manifest.entries.removed(attribute.key))

case class Manifest(entries: Map[Text, Text]):
  def apply[key <: Label: DecodableManifest](attribute: ManifestAttribute[key])
  :   Optional[key.Topic] =

    if entries.defines(attribute.key) then key.decoded(entries(attribute.key)) else Unset


  def serialize: Data =
    val manifest = juj.Manifest()

    entries.stdlib.each: (key, value) =>
      manifest.getMainAttributes.nn.putValue(key.s, value.s)

    val out = ji.ByteArrayOutputStream()
    manifest.write(out)
    out.toByteArray().nn.immutable(using Unsafe)
