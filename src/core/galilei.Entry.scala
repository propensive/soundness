package galilei

trait Entry

trait WindowsEntry extends Entry
trait UnixEntry extends Entry

trait File
trait Directory
trait Fifo
trait BlockDevice
trait CharDevice

case object File extends WindowsEntry, UnixEntry
case object Directory extends WindowsEntry, UnixEntry
case object BlockDevice extends UnixEntry
case object CharDevice extends UnixEntry
case object Fifo extends UnixEntry
