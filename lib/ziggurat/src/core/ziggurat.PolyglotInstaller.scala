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
package ziggurat

import anticipation.*
import contingency.*
import distillate.*
import galilei.*
import gossamer.*
import hellenism.*
import hieroglyph.*
import monotonous.*, alphabets.base64.standard
import prepositional.*
import proscenium.*
import serpentine.*
import turbulence.*
import vacuous.*

import classloaders.threadContext
import charDecoders.utf8
import charEncoders.utf8 as utf8Encoder
import textSanitizers.skip

import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled
import filesystemOptions.overwritePreexisting.enabled
import filesystemOptions.createNonexistent.enabled
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.deleteRecursively.enabled

object PolyglotInstaller:
  private val ChunkSize: Int = 8000
  private val RunnerPrefix = t"runner-"
  private val ExeSuffix = t".exe"
  private val DataName = t"data"

  def bundle(payloads: List[Payload]): Data =
    val template = cp"/ziggurat/polyglot.tmpl".read[Text]
    val bat      = cp"/ziggurat/polyglot.bat".read[Text]
    val ps1      = cp"/ziggurat/polyglot.ps1".read[Text]
    val sh       = cp"/ziggurat/polyglot.sh".read[Text]

    val prefix: Text =
      template.cut(t"@@BAT@@").join(bat).cut(t"@@PS1@@").join(ps1).cut(t"@@SH@@").join(sh)

    val encoded: List[(Text, Text)] = payloads.map: payload =>
      val raw: Data =
        if !payload.gzip then payload.bytes
        else Stream(payload.bytes).compress[Gzip].read[Data]

      payload.label -> raw.serialize[Base64].slices(ChunkSize).join(t"", t"\n", t"\n")

    val builder = StringBuilder()
    builder.add(prefix)
    if !prefix.ends(t"\n") then builder.add('\n')

    var offset = 1
    val indexEntries = encoded.map: (label, content) =>
      val entry = t"$label=$offset"
      offset = offset + content.count(_ == '\n') + 2
      entry

    builder.add(t"index:")
    builder.add(indexEntries.join(t","))
    builder.add('\n')

    encoded.foreach: (_, content) =>
      builder.add(t"-----BEGIN CERTIFICATE-----\n")
      builder.add(content)
      builder.add(t"-----END CERTIFICATE-----\n")

    builder.add(t"#>\n")
    builder.text.data(using utf8Encoder)


  def main(args: Array[String]): Unit = unsafely:
    if args.length != 2 then
      System.err.nn.println("usage: ziggurat.PolyglotInstaller <output-file> <staging-dir>")
      System.exit(1)

    val output: Path on Linux = args(0).tt.decode[Path on Linux]
    val staging: Path on Linux = args(1).tt.decode[Path on Linux]

    val children: List[Path on Linux] = staging.children.to(List)

    val runnerPayloads: List[Payload] =
      children
      . filter(_.name.starts(RunnerPrefix))
      . sortBy(_.name.s)
      . map: path =>
          val name = path.name
          val withoutPrefix = name.skip(RunnerPrefix.length)
          val label = if withoutPrefix.ends(ExeSuffix)
                      then withoutPrefix.skip(ExeSuffix.length, Rtl)
                      else withoutPrefix
          val data: Data = path.open(_.read[Data])
          val gzip = !label.starts(t"windows")
          Payload(label, data, gzip)

    val dataPath: Path on Linux = staging/DataName

    val dataPayload: Optional[Payload] =
      if dataPath.exists() then
        val bytes: Data = dataPath.open(_.stream[Data].read[Data])
        Payload(DataName, bytes, gzip = false)
      else Unset

    val payloads = runnerPayloads ++ dataPayload.option
    val bundleBytes: Data = bundle(payloads)

    output.create[File]()
    output.open: handle =>
      Stream(bundleBytes).writeTo(handle)

    output.executable() = true
