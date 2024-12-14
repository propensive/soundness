package galilei

import anticipation.*
import contingency.*
import gossamer.*
import nomenclature.*
import prepositional.*
import serpentine.*
import spectacular.*

object DosDrive:
  export Dos.navigable

case class DosDrive(letter: Char) extends Root(t"$letter:\\", t"\\", Case.Upper):
  type Platform = Dos
