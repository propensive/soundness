package zeppelin

import anticipation.*
import fulminate.*
import contingency.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import turbulence.*
import vacuous.*

import java.util.zip as juz

object ZipStream:
  def apply[SourceType: Readable by Bytes](source: SourceType): ZipStream =
    new ZipStream(() => source.stream[Bytes], { _ => true })

class ZipStream(stream: () => LazyList[Bytes], filter: ZipRef => Boolean):
  def keep(predicate: ZipRef => Boolean): ZipStream =
    new ZipStream(stream, { (ref: ZipRef) => filter(ref) && predicate(ref) })

  def extract(ref: ZipRef): ZipEntry raises ZipError =
    safely(keep(_ == ref).map(identity(_)).headOption.getOrElse(Unset)).or:
      abort(ZipError(ref.show))

  def each(lambda: ZipEntry => Unit): Unit raises ZipError = map[Unit](lambda)

  def map[ElementType](lambda: ZipEntry => ElementType): LazyList[ElementType] raises ZipError =
    val zipIn = juz.ZipInputStream(LazyListInputStream(stream()))

    def recur(): LazyList[ZipEntry] = zipIn.getNextEntry() match
      case null                         => LazyList()
      case entry if entry.isDirectory() => recur()
      case entry =>
        import exceptionDiagnostics.empty
        val ref = tend { case PathError(path, _) => ZipError(path) }.within:
          ZipRef(entry.getName().nn.tt)

        if !filter(ref) then recur() else
          ZipEntry(ref, LazyList(Bytes.construct(entry.getSize.toInt): array =>
            def read(done: Int = 0): Unit =
              if done < array.length then read(done + zipIn.read(array, done, zipIn.available()))

            read())) #:: recur()

    recur().map(lambda)
