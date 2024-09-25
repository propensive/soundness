package galilei

import anticipation.*
import contingency.*
import prepositional.*
import rudiments.*
import serpentine.*

package pathNavigation:
  given (using Tactic[PathError]) => %.type is Navigable by Text as unix = %.navigable
  
  given (using Tactic[PathError]) => WindowsDrive is Navigable by Text as windows =
    WindowsDrive.navigable

@targetName("UnixRoot")
final val `%`: Pathlike & Galilei.%.type = Galilei.%

val C: WindowsDrive = WindowsDrive('C')
val D: WindowsDrive = WindowsDrive('D')