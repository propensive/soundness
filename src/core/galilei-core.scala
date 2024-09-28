package galilei

import java.io as ji

import contingency.*
import prepositional.*
import rudiments.*
import nomenclature.*
import serpentine.*
import spectacular.*
import anticipation.*

package pathNavigation:
  export Unix.navigable as unix
  export Windows.navigable as windows

final val C: WindowsDrive = WindowsDrive('C')
final val D: WindowsDrive = WindowsDrive('D')

@targetName("UnixRoot")
final val `%`: UnixRoot = UnixRootSingleton

extension [ElementType, PlatformType <: Filesystem: Navigable by ElementType]
    (path: Path on PlatformType)
  def open[ResultType](lambda: File => ResultType)(using Encoder[Path on PlatformType])
          : ResultType =
    val file = File(ji.FileInputStream(ji.File(path.encode.s)))
    try lambda(file) finally file.close()