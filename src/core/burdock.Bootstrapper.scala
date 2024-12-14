package burdock

import ambience.*
import anticipation.*
import contingency.*
import digression.*
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

  given ("Burdock-Main" is EncodableManifest of Fqcn) as burdockMain = _.text
  
  given ("Burdock-Require" is EncodableManifest of List[Requirement]) as burdockRequire =
    _.map(_.text).join(t" ")
  
  given ("Burdock-Verbosity" is EncodableManifest of Text) as burdockVerbosity = identity(_)
  
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
        val data = url.get().read[Bytes]
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

        manifest2 - MainClass + require + burdockMain + verbosity + MainClass(fqcn"burdock.Bootstrap")
      
      val tmpFile = jarfile.parent.vouch(using Unsafe) / Name(jarfile.name.vouch(using Unsafe).text+t".tmp")
      
      Zipfile.write(tmpFile):
        ZipEntry(Path.parse[Zip](t"META-INF/MANIFEST.MF"), manifest3.serialize) #::
          todo.sift[Entry].to(LazyList).map: entry =>
            ZipEntry(Path.parse[Zip](entry.name), () => LazyList(entry.data))
      
      import filesystemOptions.overwritePreexisting.enabled
      import filesystemOptions.deleteRecursively.disabled
      import filesystemOptions.moveAtomically.enabled
      import filesystemOptions.createNonexistentParents.disabled

      tmpFile.moveTo(jarfile)
      
      Exit.Ok

