package galilei

import anticipation.*
import contingency.*
import gossamer.*
import nomenclature.*
import prepositional.*
import serpentine.*
import spectacular.*

object WindowsDrive:
  export Windows.navigable

case class WindowsDrive(letter: Char) extends Root(t"$letter:\\", t"\\", Case.Preserving):
  type Platform = Windows
