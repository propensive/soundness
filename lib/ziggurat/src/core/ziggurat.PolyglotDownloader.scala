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
import prepositional.*
import proscenium.*
import serpentine.*
import turbulence.*

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

object PolyglotDownloader:
  def bundle(url: Text, hash: Text): Data =
    val template = cp"/ziggurat/polyglot.tmpl".read[Text]
    val bat      = cp"/ziggurat/polyglot-download.bat".read[Text]
    val ps1      = cp"/ziggurat/polyglot-download.ps1".read[Text]
    val sh       = cp"/ziggurat/polyglot-download.sh".read[Text]

    val prefix: Text =
      template
      . cut(t"@@BAT@@").join(bat)
      . cut(t"@@PS1@@").join(ps1)
      . cut(t"@@SH@@").join(sh)

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


  def main(args: Array[String]): Unit = unsafely:
    if args.length != 3 then
      System.err.nn.println("usage: ziggurat.PolyglotDownloader <output-file> <url> <sha256>")
      System.exit(1)

    val output: Path on Linux = args(0).tt.decode[Path on Linux]
    val url: Text = args(1).tt
    val hash: Text = args(2).tt

    val bundleBytes: Data = bundle(url, hash)

    output.create[File]()
    output.open: handle =>
      Stream(bundleBytes).writeTo(handle)

    output.executable() = true
