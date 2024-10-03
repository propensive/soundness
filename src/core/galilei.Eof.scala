package galilei

import java.nio.file as jnf

import prepositional.*
import serpentine.*

object Eof:
  given [FileType]
      (using openable: FileType is Openable by jnf.OpenOption)
      => Eof[FileType] is Openable by jnf.OpenOption into openable.Result as openable =
    new Openable:
      type Self = Eof[FileType]
      type Operand = jnf.OpenOption
      type Result = openable.Result
      protected type Carrier = openable.Carrier

      def init(eof: Eof[FileType], options: List[Operand]): Carrier =
        openable.init(eof.file, jnf.StandardOpenOption.APPEND :: options)
    
      def handle(carrier: Carrier): Result = openable.handle(carrier)
      def close(carrier: Carrier): Unit = openable.close(carrier)

case class Eof[FileType](file: FileType)