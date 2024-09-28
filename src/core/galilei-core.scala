package galilei

import java.io as ji

import contingency.*
import prepositional.*
import rudiments.*
import nomenclature.*
import serpentine.*
import spectacular.*

erased trait OperatingSystem

erased trait Windows extends OperatingSystem

object Unix:
  export UnixRoot.navigable

object Windows:
  export WindowsDrive.navigable

erased trait Unix extends OperatingSystem

package pathNavigation:
  export UnixRoot.navigable as unix
  export WindowsDrive.navigable as windows

final val C: WindowsDrive = WindowsDrive('C')
final val D: WindowsDrive = WindowsDrive('D')

@targetName("UnixRoot")
final val `%`: UnixRoot = UnixRootSingleton

extension [ElementType, PlatformType <: OperatingSystem: Navigable by ElementType](path: Path on PlatformType)
  def open[ResultType](lambda: File => ResultType)(using Encoder[Path on PlatformType])
          : ResultType =
    val file = File(ji.FileInputStream(ji.File(path.encode.s)))
    try lambda(file) finally file.close()