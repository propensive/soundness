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
import galilei.*, galilei.Platform.pathReadable
import gossamer.*
import hellenism.*
import hieroglyph.*
import monotonous.*, alphabets.base64Standard
import prepositional.*
import serpentine.*
import turbulence.*
import vacuous.*

import charDecoders.utf8Decoder
import charEncoders.utf8Encoder
import classloaders.threadContextClassloader
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.deleteRecursively.enabled
import filesystemOptions.overwritePreexisting.enabled
import textSanitizers.skipSanitizer

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

    encoded.foreach: (_, content) =>
      builder.add(t"-----BEGIN CERTIFICATE-----\n")
      builder.add(content)
      builder.add(t"-----END CERTIFICATE-----\n")

    builder.add(t"#>\n")
    builder.text.data(using charEncoders.utf8Encoder)


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
    builder.text.data(using charEncoders.utf8Encoder)


  // The polyglot online launcher. Unlike `installer` (which embeds every bare stub), this
  // embeds only the application JAR — once, as the `data` payload, exactly as `installer`
  // does — plus an `assets:` table of `label=url|hash`. At runtime the launcher downloads
  // the one bare runner stub it needs, verifies its SHA-256, appends the embedded JAR, then
  // replaces itself and execs. `entries` are `(label, stub-url, stub-sha256)`.
  def onlineLauncher(jar: Data, entries: List[(Text, Text, Text)]): Data =
    val template = cp"/ziggurat/xeq.tmpl".read[Text]
    val bat      = cp"/ziggurat/xeq-onlinelauncher.bat".read[Text]
    val ps1      = cp"/ziggurat/xeq-onlinelauncher.ps1".read[Text]
    val sh       = cp"/ziggurat/xeq-onlinelauncher.sh".read[Text]

    val prefix: Text =
      template.cut(t"@@BAT@@").join(bat).cut(t"@@PS1@@").join(ps1).cut(t"@@SH@@").join(sh)

    // The JAR is embedded uncompressed, mirroring the installer's `data` payload, so the
    // same `index:`/`-----BEGIN CERTIFICATE-----` extraction logic decodes it.
    val content: Text = jar.serialize[Base64].slices(ChunkSize).join(t"", t"\n", t"\n")

    val rows: List[Text] = entries.map: (label, url, hash) =>
      t"$label=$url|$hash"

    val builder = StringBuilder()
    builder.add(prefix)
    if !prefix.ends(t"\n") then builder.add('\n')
    builder.add(t"index:$DataName=1\n")
    builder.add(t"-----BEGIN CERTIFICATE-----\n")
    builder.add(content)
    builder.add(t"-----END CERTIFICATE-----\n")
    builder.add(t"assets:")
    builder.add(rows.join(t","))
    builder.add('\n')
    builder.add(t"#>\n")
    builder.text.data(using charEncoders.utf8Encoder)


  private def write(output: Path on Linux, data: Data): Unit = unsafely:
    output.create[File]()

    output.write(data)
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

          val data: Data = path.read[Data]
          val gzip = !label.starts(t"windows")
          Payload(label, data, gzip)

    val dataPath: Path on Linux = staging/DataName

    val dataPayload: Optional[Payload] =
      if dataPath.exists() then
        val bytes: Data = dataPath.read[Data]
        Payload(DataName, bytes, gzip = false)
      else
        Unset

    write(outputPath, installer(runnerPayloads ++ dataPayload.option))


  private def downloaderMain(output: Text, url: Text, hash: Text): Unit = unsafely:
    write(output.decode[Path on Linux], downloader(url, hash))


  // Builds an online launcher from a JAR plus a runner manifest (`label<TAB>sha256` per
  // line, e.g. `etc/runners/<version>.tsv`). Each stub's download URL is `<baseUrl>/runner-
  // <label>[.exe]`, where `baseUrl` is the published `runners-<version>` release.
  private def onlineLauncherMain(output: Text, jar: Text, manifest: Text, baseUrl: Text): Unit =
    unsafely:
      val outputPath: Path on Linux = output.decode[Path on Linux]
      val jarData: Data = jar.decode[Path on Linux].read[Data]
      val base: Text = if baseUrl.ends(t"/") then baseUrl else t"$baseUrl/"

      val entries: List[(Text, Text, Text)] =
        manifest.decode[Path on Linux].read[Text].cut(t"\n").map(_.trim)
        . filter(_ != t"")
        . map: line =>
            val fields: List[Text] = line.cut(t"\t").to(List)
            val label: Text = fields.head
            val hash: Text = fields.last

            val name: Text =
              if label.starts(t"windows") then t"runner-$label.exe" else t"runner-$label"

            (label, t"$base$name", hash)

      write(outputPath, onlineLauncher(jarData, entries))


  def main(args: Array[String]): Unit =
    args.iterator.toList match
      case "installer" :: output :: staging :: Nil =>
        installerMain(output.tt, staging.tt)

      case "downloader" :: output :: url :: hash :: Nil =>
        downloaderMain(output.tt, url.tt, hash.tt)

      case "onlinelauncher" :: output :: jar :: manifest :: base :: Nil =>
        onlineLauncherMain(output.tt, jar.tt, manifest.tt, base.tt)

      case _ =>
        System.err.nn.println("usage: ziggurat.Xeq installer <output-file> <staging-dir>")
        System.err.nn.println("       ziggurat.Xeq downloader <output-file> <url> <sha256>")

        System.err.nn.println(
          "       ziggurat.Xeq onlinelauncher <output-file> <jar> <manifest.tsv> <base-url>")

        System.exit(1)
