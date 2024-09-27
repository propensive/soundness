package galilei

import contingency.*
import prepositional.*
import rudiments.*
import nomenclature.*
import serpentine.*


erased trait Windows

object Unix:
  export UnixRoot.navigable

erased trait Unix

package pathNavigation:
  export UnixRoot.navigable as unix
  export WindowsDrive.navigable as windows

final val C: WindowsDrive = WindowsDrive('C')
final val D: WindowsDrive = WindowsDrive('D')

@targetName("UnixRoot")
final val `%`: UnixRoot = UnixRoot()