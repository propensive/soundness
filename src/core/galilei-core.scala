package galilei

import anticipation.*
import contingency.*
import rudiments.*
import serpentine.*
import prepositional.*

package pathNavigation:
  given (using Tactic[PathError]) => %.type is Navigable by Text as unix = %.navigable
  
  given (using Tactic[PathError]) => WindowsDrive is Navigable by Text as windows =
    WindowsDrive.navigable

export Galilei.`%`

val C: WindowsDrive = WindowsDrive('C')
val D: WindowsDrive = WindowsDrive('D')