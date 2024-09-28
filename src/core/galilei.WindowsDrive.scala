package galilei

import anticipation.*
import contingency.*
import nomenclature.*
import gossamer.*
import prepositional.*
import spectacular.*
import serpentine.*

object WindowsDrive:
  export Windows.navigable

case class WindowsDrive(letter: Char) extends Root(t"$letter:\\", t"\\", Case.Preserving):
  type Platform = Windows
