package burdock

import anticipation.*
import prepositional.*
import gossamer.*
import turbulence.*
import nomenclature.*
import fulminate.*
import zeppelin.*
import rudiments.*
import serpentine.*
import galilei.*
import contingency.*
import gastronomy.*
import nettlesome.*
import spectacular.*
import exoskeleton.*
import monotonous.*
import revolution.*
import telekinesis.*
import ambience.*
import digression.*
import eucalyptus.*
import vacuous.*

import stdioSources.virtualMachine.ansi
import unhandledErrors.stackTrace
import executives.direct
import parameterInterpretation.posix
import pathNavigation.posix
import workingDirectories.virtualMachine
import systemProperties.virtualMachine
import internetAccess.enabled
import logging.stderr
import logFormats.ansiStandard
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
  
  case class UserError(detail: Message)(using Diagnostics) extends Error(detail)
  case class Entry(name: Text, data: Bytes)

  def main(args: IArray[Text]): Unit = application(args):
    mend:
      case error: Error =>
        Err.println(error.message)
        Exit.Fail(1)
    .within:
      val jarfile = args.prim.let(workingDirectory[Path on Posix].resolve(_)).or:
        abort(UserError(m"Please specify the JAR file to update as the first parameter"))
      
      if !jarfile.exists() then abort(UserError(m"The file $jarfile does not exist"))
      val classpath = arguments.drop(1).map(_()).map(workingDirectory[Path on Posix].resolve(_))
      
      val urls = classpath.map: entry =>
        entry.ancestor(6).let: base =>
          (url"https://repo1.maven.org/" + entry.relativeTo(base).on[HttpUrl]).show.decode[HttpUrl]
      
      val entries: Map[(Text, Text), Requirement] = urls.compact.flatMap: url =>
        val data = url.get().read[Bytes]
        val digest = data.digest[Sha2[256]].serialize[Hex]

        ZipStream(data).keep(_.text != t"META-INF/MANIFEST.MF").map: entry =>
          (entry.ref.show, entry.checksum[Sha2[256]].serialize[Hex]) -> Requirement(url, digest)
      .to(Map)


      val todo: List[Requirement | Entry | Manifest] = jarfile.open: handle =>
        ZipStream(handle.read[Bytes]).map: entry =>
          if entry.ref.show == t"META-INF/MANIFEST.MF" then entry.read[Bytes].read[Manifest]
          else entries.at((entry.ref.show, entry.checksum[Sha2[256]].serialize[Hex])).or:
            Entry(entry.ref.show, entry.read[Bytes])
        .to(List)
      
      val manifest = todo.sift[Manifest].prim.or:
        abort(UserError(m"There is no META-INF/MANIFEST.MF entry in the JAR file"))

      val manifest2 =
        import manifestAttributes.*
        val require = BurdockRequire(todo.sift[Requirement].to(Set).to(List))

        val burdockMain = manifest(MainClass).let(BurdockMain(_)).or:
          abort(UserError(m"Manifest file did not contain a Main-Class entry"))

        val verbosity = BurdockVerbosity(t"silent")

        manifest - MainClass + require + burdockMain + verbosity + MainClass(fqcn"burdock.Bootstrap")
      
      val tmpFile = jarfile.parent.vouch(using Unsafe) / Name(jarfile.name.vouch(using Unsafe).text+t".tmp")
      
      Zipfile.write(tmpFile)
       (ZipEntry
         (Path.parse[Zip](t"META-INF/MANIFEST.MF"),
          manifest2.serialize) #:: todo.sift[Entry].to(LazyList).map: entry =>
            ZipEntry(Path.parse[Zip](entry.name), () => LazyList(entry.data)))
      
      import filesystemOptions.overwritePreexisting.enabled
      import filesystemOptions.deleteRecursively.disabled
      import filesystemOptions.moveAtomically.enabled
      import filesystemOptions.createNonexistentParents.disabled
      tmpFile.moveTo(jarfile)
      
      Exit.Ok


