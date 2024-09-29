package galilei

sealed trait Entry

sealed trait WindowsEntry extends Entry
sealed trait UnixEntry extends Entry

case object File extends WindowsEntry, UnixEntry
case object Directory extends WindowsEntry, UnixEntry
case object Symlink extends WindowsEntry, UnixEntry
case object BlockDevice extends UnixEntry
case object CharDevice extends UnixEntry
case object Fifo extends UnixEntry
case object Socket extends UnixEntry
