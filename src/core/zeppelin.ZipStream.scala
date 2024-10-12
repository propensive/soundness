package zeppelin

import anticipation.*
import fulminate.*
import contingency.*
import prepositional.*
import rudiments.*
import serpentine.*
import nomenclature.*
import turbulence.*
import vacuous.*

import java.util.zip as juz

object ZipStream:
  def apply[SourceType: Readable by Bytes](source: SourceType): ZipStream logs Text =
    new ZipStream(() => source.stream[Bytes], _ => true)

class ZipStream(stream: () => LazyList[Bytes], filter: (Path on Zip) => Boolean):

  def keep(predicate: (Path on Zip) => Boolean): ZipStream =
    new ZipStream(stream, { (ref: Path on Zip) => filter(ref) && predicate(ref) })

  def extract(ref: Zip.ZipRoot => Path on Zip): ZipEntry raises ZipError logs Text =
    val root = Zip.ZipRoot()
    safely(keep(_.descent == ref(root).descent).map(identity(_)).headOption.get).or:
      abort(ZipError())

  def each(lambda: ZipEntry => Unit): Unit raises ZipError = map[Unit](lambda)

  def map[ElementType](lambda: ZipEntry => ElementType): LazyList[ElementType] raises ZipError =
    val zipIn = juz.ZipInputStream(LazyListInputStream(stream()))

    def recur(): LazyList[ZipEntry] =
     zipIn.getNextEntry() match
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
          def read(): LazyList[Bytes] =
            if zipIn.available == 0 then LazyList() else
              val size = entry.getSize.toInt.min(4096).puncture(-1).or(4096)
              val array: Array[Byte] = new Array[Byte](size)
              val count = zipIn.read(array, 0, array.length)
              if count == 0 then LazyList()
              else (if count < size then array.take(count) else array).immutable(using Unsafe) #:: read()
            
          ZipEntry(ref, read()) #:: recur()

    recur().map(lambda)
