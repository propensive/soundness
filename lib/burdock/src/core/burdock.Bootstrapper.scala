                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                    ╭───╮                                         ┃
┃  ╭─────────╮                                       │   │                                         ┃
┃  │   ╭─────╯╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮  ┃
┃  │   ╰─────╮│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯  ┃
┃  ╰─────╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ├╌╯╌─╯╰─╌ ╰───╮╰─╌ ╰───╮  ┃
┃  ╭─────╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╌   │╭───╌   │  ┃
┃  ╰─────────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯  ┃
┃                                                                                                  ┃
┃    Soundness, version 0.27.0. © Copyright 2023-25 Jon Pretty, Propensive OÜ.                     ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        http://www.apache.org/licenses/LICENSE-2.0                                                ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package burdock

import ambience.*
import anticipation.*
import contingency.*
import digression.*
import distillate.*
import eucalyptus.*
import exoskeleton.*
import fulminate.*
import galilei.*
import gastronomy.*
import gossamer.*
import hellenism.*
import monotonous.*
import nettlesome.*
import nomenclature.*
import parasite.*
import prepositional.*
import proscenium.*
import revolution.*
import rudiments.*
import serpentine.*
import spectacular.*
import telekinesis.*
import turbulence.*
import vacuous.*
import zeppelin.*

import stdioSources.virtualMachine.ansi
import unhandledErrors.stackTrace
import executives.direct
import parameterInterpretation.posix
import pathNavigation.posix
import workingDirectories.virtualMachine
import systemProperties.virtualMachine
import internetAccess.enabled
import logging.silent
import alphabets.hex.lowerCase
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.disabled
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.createNonexistent.disabled

object Bootstrapper:
  object BurdockMain extends ManifestAttribute["Burdock-Main"]
  object BurdockVerbosity extends ManifestAttribute["Burdock-Verbosity"]
  object BurdockRequire extends ManifestAttribute["Burdock-Require"]

  given burdockMain: ("Burdock-Main" is EncodableManifest of Fqcn) = _.text

  given burdockRequire: ("Burdock-Require" is EncodableManifest of List[Requirement]) =
    _.map(_.text).join(t" ")

  given burdockVerbosity: ("Burdock-Verbosity" is EncodableManifest of Text) = identity(_)

  case class Requirement(url: HttpUrl, digest: Text):
    def text = t"$digest:$url"

  case class Entry(name: Text, data: Bytes)

  case class UserError(detail: Message)(using Diagnostics) extends Error(detail)

  def main(args: IArray[Text]): Unit = application(args):
    mend:
      case error: Error =>
        Err.println(error.message)
        Exit.Fail(1)

    . within:
        val jarfile: Path on Posix =
          ClassRef(Class.forName("burdock.Bootstrap").nn).classpathEntry match
            case ClasspathEntry.Jar(file) =>
              Path.parse(file)

            case other =>
              abort(UserError(m"Could not determine location of bootstrap class"))

        Out.println(m"Bootstrapping JAR file $jarfile")

        if !jarfile.exists() then abort(UserError(m"The file $jarfile does not exist"))
        val classpath = arguments.map(_()).map(workingDirectory[Path on Posix].resolve(_))

        val urls = classpath.map: entry =>
          entry.ancestor(6).let: base =>
            if base.name == n"repo1.maven.org" && base.parent.let(_.name) == n"https"
            then
              val urlPath = url"https://repo1.maven.org/" + entry.relativeTo(base).on[HttpUrl]
              urlPath.show.decode[HttpUrl]
            else
              Out.println(m"Cannot resolve online location of $entry")
              Unset

        val entries: Map[(Text, Text), Requirement] = urls.compact.flatMap: url =>
          Out.println(m"Downloading $url")
          val data = url.fetch().read[Bytes]
          val digest = data.digest[Sha2[256]].serialize[Hex]

          def filter(name: Text): Boolean =
            name == t"burdock/Bootstrap.class" || name != t"META-INF/MANIFEST.MF"

          ZipStream(data).keep(_.text != t"META-INF/MANIFEST.MF").map: entry =>
            (entry.ref.show, entry.checksum[Sha2[256]].serialize[Hex]) -> Requirement(url, digest)

        . to(Map)

        val manifest: Promise[Manifest] = Promise()

        val todo: List[Requirement | Entry] = jarfile.open: handle =>
          ZipStream(handle.read[Bytes]).map: entry =>
            if entry.ref.show == t"META-INF/MANIFEST.MF"
            then manifest.fulfill(entry.read[Bytes].read[Manifest]) yet Unset
            else if entry.ref.show == t"burdock/Bootstrap.class"
            then Entry(entry.ref.show, entry.read[Bytes])
            else entries.at((entry.ref.show, entry.checksum[Sha2[256]].serialize[Hex])).or:
              Entry(entry.ref.show, entry.read[Bytes])

          . to(List).compact

        val manifest2 = manifest().or:
          abort(UserError(m"There is no META-INF/MANIFEST.MF entry in the JAR file"))

        val manifest3 =
          import manifestAttributes.*
          val require = BurdockRequire(todo.sift[Requirement].to(Set).to(List))

          val burdockMain = manifest2(MainClass).let(BurdockMain(_)).or:
            abort(UserError(m"Manifest file did not contain a Main-Class entry"))

          val verbosity = BurdockVerbosity(t"silent")

          manifest2 - MainClass + require + burdockMain + verbosity
          + MainClass(fqcn"burdock.Bootstrap")

        val tmpFile = jarfile.parent.vouch / Name(jarfile.name.vouch.text+t".tmp")

        Zipfile.write(tmpFile):
          ZipEntry(Path.parse[Zip](t"META-INF/MANIFEST.MF"), manifest3.serialize) #::
            todo.sift[Entry].to(Stream).map: entry =>
              ZipEntry(Path.parse[Zip](entry.name), () => Stream(entry.data))

        import filesystemOptions.overwritePreexisting.enabled
        import filesystemOptions.deleteRecursively.disabled
        import filesystemOptions.moveAtomically.enabled
        import filesystemOptions.createNonexistentParents.disabled

        tmpFile.moveTo(jarfile)

        Exit.Ok
