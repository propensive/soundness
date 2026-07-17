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
package zeppelin

import java.nio.channels as jnc
import java.nio.file as jnf

import anticipation.*
import aperture.*
import contingency.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

// The form for Java archives: `path.open[Jar]()`. A JAR is a ZIP, so a `JarHandle` is a
// `ZipHandle` refined with the archive's parsed manifest; entries and their content behave
// exactly as they do for the `Zip` form.
trait Jar

object Jar:
  private val ManifestName: Text = t"META-INF/MANIFEST.MF"

  class JarHandle private[zeppelin] (zipfile: Zipfile) extends ZipHandle(zipfile):

    // The main attributes of `META-INF/MANIFEST.MF`: continuation lines (a leading space)
    // rejoin the preceding attribute, and parsing stops at the first blank line, which ends
    // the main section. An archive without a manifest has no attributes.
    def manifest: Map[Text, Text] =
      zipfile.entries.find(_.ref.encode == ManifestName).map: entry =>
        val bytes: Array[Byte] =
          entry.contents.foldLeft(Array.empty[Byte]) { (acc, data) => acc ++ data.mutable(using Unsafe) }

        val text: Text = String(bytes, "UTF-8").tt
        val unfolded = text.s.split("\r\n|\r|\n", -1).nn.map(_.nn)
        val main = unfolded.takeWhile(_.nonEmpty)

        val rejoined = main.foldLeft(List.empty[String]): (acc, line) =>
          if line.startsWith(" ") && acc.nonEmpty then (acc.head + line.drop(1)) :: acc.tail
          else line :: acc

        rejoined.reverse.flatMap: line =>
          line.indexOf(": ") match
            case -1    => Nil
            case index => List((line.take(index).tt, line.drop(index + 2).tt))
        . to(Map)

      . getOrElse(Map())

  // A named class rather than an anonymous given instance, for the reasons documented on
  // galilei's `FileOpenable`. Like `Zip`, archives open read-only for now.
  class JarOpenable[path: Abstractable across Paths to Text](using Tactic[ZipError])
  extends Openable:

    type Self = path
    type Form = Jar
    type Operand = Nothing
    type Result = JarHandle

    def open[grants <: Grant, result]
      ( value: path, mode: Mode granting grants, flags: List[Nothing] )
      ( block: (JarHandle & Granting[grants]) ?=> result )
    :   result =

      if mode.atoms.contains(Write) then abort(ZipError(ZipError.Reason.WriteUnsupported))

      val channel =
        jnc.FileChannel.open(jnf.Path.of(value.generic.s), jnf.StandardOpenOption.READ).nn

      try
        val zipfile = Zipfile.parse(Zipfile.ChannelSource(channel))
        block(using new JarHandle(zipfile) with Granting[grants] {})
      finally channel.close()

  class JarDataOpenable(using Tactic[ZipError]) extends Openable:
    type Self = Data
    type Form = Jar
    type Operand = Nothing
    type Result = JarHandle

    def open[grants <: Grant, result]
      ( value: Data, mode: Mode granting grants, flags: List[Nothing] )
      ( block: (JarHandle & Granting[grants]) ?=> result )
    :   result =

      if mode.atoms.contains(Write) then abort(ZipError(ZipError.Reason.WriteUnsupported))
      block(using new JarHandle(Zipfile.parse(Zipfile.DataSource(value))) with Granting[grants] {})

  given openable: [path: Abstractable across Paths to Text]
  =>  Tactic[ZipError]
  =>  JarOpenable[path] =
    JarOpenable[path]

  given dataOpenable: Tactic[ZipError] => JarDataOpenable = JarDataOpenable()
