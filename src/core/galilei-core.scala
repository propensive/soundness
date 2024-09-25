package galilei

import anticipation.*
import contingency.*
import prepositional.*
import rudiments.*
import serpentine.*

package pathNavigation:
  given (using Tactic[PathError]) => UnixRoot is Navigable by Text as unix = UnixRoot.navigable
  
  given (using Tactic[PathError]) => WindowsDrive is Navigable by Text as windows =
    WindowsDrive.navigable

final val C: WindowsDrive = WindowsDrive('C')
final val D: WindowsDrive = WindowsDrive('D')

@targetName("UnixRoot")
final val `%`: UnixRoot = UnixRoot()