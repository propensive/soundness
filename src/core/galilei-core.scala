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
  export Linux.navigable as linux
  export Windows.navigable as windows

final val C: WindowsDrive = WindowsDrive('C')
final val D: WindowsDrive = WindowsDrive('D')

@targetName("LinuxRoot")
final val `%`: Linux.Root = Linux.RootSingleton

@targetName("MacOsRoot")
final val `$`: MacOs.Root = MacOs.RootSingleton

extension [PlatformType <: Filesystem: Navigable](path: Path on PlatformType)
  def open[ResultType](lambda: File => ResultType): ResultType =
    val file = File(ji.FileInputStream(ji.File(path.encode.s)))
    try lambda(file) finally file.close()