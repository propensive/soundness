package zeppelin

import anticipation.*
import fulminate.*
import contingency.*
import prepositional.*
import gossamer.*
import eucalyptus.*
import rudiments.*
import serpentine.*
import nomenclature.*
import turbulence.*
import vacuous.*

import java.util.zip as juz

given Realm = realm"zeppelin"

object ZipStream:
  def apply[SourceType: Readable by Bytes](source: SourceType): ZipStream logs Text =
    Log.info(t"Constrictung new zip stream")
    new ZipStream(() => source.stream[Bytes], { _ => true })

class ZipStream(stream: () => LazyList[Bytes], filter: (Path on Zip) => Boolean):
  def keep(predicate: (Path on Zip) => Boolean): ZipStream =
    new ZipStream(stream, { (ref: Path on Zip) => filter(ref) && predicate(ref) })

  def extract(ref: Zip.ZipRoot => Path on Zip): ZipEntry raises ZipError logs Text =
    Log.info(t"EXTRACTING")
    val root = Zip.ZipRoot()
    unsafely:
      keep: path =>
        Log.info(t"Comparing ${path.descent} with ${ref(root).descent}")
        path.descent == ref(root).descent
      .map(identity(_)).headOption.get
    ////abort(ZipError())

  def each(lambda: ZipEntry => Unit): Unit raises ZipError = map[Unit](lambda)

  def map[ElementType](lambda: ZipEntry => ElementType): LazyList[ElementType] raises ZipError =
    val zipIn = juz.ZipInputStream(LazyListInputStream(stream()))

    def recur(): LazyList[ZipEntry] = zipIn.getNextEntry() match
      case null                         => LazyList()
      case entry if entry.isDirectory() => recur()
      case entry =>
        import errorDiagnostics.empty
        val ref: Path on Zip =
          tend:
            case PathError(reason, path) => ZipError()
            case NameError(_, _, _)      => ZipError()
          .within:
            Path.parse[Zip](entry.getName().nn.tt)

        if !filter(ref) then recur() else
          ZipEntry(ref, LazyList(Bytes.construct(entry.getSize.toInt): array =>
            def read(done: Int = 0): Unit =
              if done < array.length then read(done + zipIn.read(array, done, zipIn.available()))

            read())) #:: recur()

    recur().map(lambda)
