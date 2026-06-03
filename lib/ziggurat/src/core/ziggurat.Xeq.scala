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
import rudiments.*
import hellenism.*
import hieroglyph.*
import monotonous.*, alphabets.base64.standard
import prepositional.*
import serpentine.*
import turbulence.*
import vacuous.*

import charDecoders.utf8
import charEncoders.utf8
import classloaders.threadContext
import filesystemOptions.createNonexistent.enabled
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.deleteRecursively.enabled
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.overwritePreexisting.enabled
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled
import textSanitizers.skip

object Xeq:
  private val ChunkSize: Int = 8000
  private val RunnerPrefix = t"runner-"
  private val ExeSuffix = t".exe"
  private val DataName = t"data"

  def installer(payloads: List[Payload]): Data =
    val template = cp"/ziggurat/xeq.tmpl".read[Text]
    val bat      = cp"/ziggurat/xeq-installer.bat".read[Text]
    val ps1      = cp"/ziggurat/xeq-installer.ps1".read[Text]
    val sh       = cp"/ziggurat/xeq-installer.sh".read[Text]

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

    encoded.each: (_, content) =>
      builder.add(t"-----BEGIN CERTIFICATE-----\n")
      builder.add(content)
      builder.add(t"-----END CERTIFICATE-----\n")

    builder.add(t"#>\n")
    builder.text.data(using charEncoders.utf8)


  def downloader(url: Text, hash: Text): Data =
    val template = cp"/ziggurat/xeq.tmpl".read[Text]
    val bat      = cp"/ziggurat/xeq-downloader.bat".read[Text]
    val ps1      = cp"/ziggurat/xeq-downloader.ps1".read[Text]
    val sh       = cp"/ziggurat/xeq-downloader.sh".read[Text]

    val prefix: Text =
      template.cut(t"@@BAT@@").join(bat).cut(t"@@PS1@@").join(ps1).cut(t"@@SH@@").join(sh)

    val builder = StringBuilder()
    builder.add(prefix)
    if !prefix.ends(t"\n") then builder.add('\n')
    builder.add(t"# URL=")
    builder.add(url)
    builder.add('\n')
    builder.add(t"# HASH=")
    builder.add(hash)
    builder.add('\n')
    builder.add(t"#>\n")
    builder.text.data(using charEncoders.utf8)


  private def write(output: Path on Linux, data: Data): Unit = unsafely:
    output.create[File]()

    output.open: handle =>
      Stream(data).writeTo(handle)

    output.executable() = true


  private def installerMain(output: Text, stagingDir: Text): Unit = unsafely:
    val outputPath: Path on Linux = output.decode[Path on Linux]
    val staging: Path on Linux = stagingDir.decode[Path on Linux]

    val children: List[Path on Linux] = staging.children.to(List)

    val runnerPayloads: List[Payload] =
      children
      . filter(_.name.starts(RunnerPrefix))
      . sortBy(_.name.s)
      . map: path =>
          val name = path.name
          val withoutPrefix = name.skip(RunnerPrefix.length)

          val label =
            if withoutPrefix.ends(ExeSuffix) then withoutPrefix.skip(ExeSuffix.length, Rtl)
            else withoutPrefix

          val data: Data = path.open(_.read[Data])
          val gzip = !label.starts(t"windows")
          Payload(label, data, gzip)

    val dataPath: Path on Linux = staging/DataName

    val dataPayload: Optional[Payload] =
      if dataPath.exists() then
        val bytes: Data = dataPath.open(_.stream[Data].read[Data])
        Payload(DataName, bytes, gzip = false)
      else
        Unset

    write(outputPath, installer(runnerPayloads ++ dataPayload.lay(Nil)(List(_))))


  private def downloaderMain(output: Text, url: Text, hash: Text): Unit = unsafely:
    write(output.decode[Path on Linux], downloader(url, hash))


  def main(args: Array[String]): Unit =
    args.iterator.to(List) match
      case "installer" :: output :: staging :: Nil =>
        installerMain(output.tt, staging.tt)

      case "downloader" :: output :: url :: hash :: Nil =>
        downloaderMain(output.tt, url.tt, hash.tt)

      case _ =>
        System.err.nn.println("usage: ziggurat.Xeq installer <output-file> <staging-dir>")
        System.err.nn.println("       ziggurat.Xeq downloader <output-file> <url> <sha256>")
        System.exit(1)
