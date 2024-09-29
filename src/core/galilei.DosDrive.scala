package galilei

import anticipation.*
import contingency.*
import nomenclature.*
import gossamer.*
import prepositional.*
import spectacular.*
import serpentine.*

object DosDrive:
  export Dos.navigable

case class DosDrive(letter: Char) extends Root(t"$letter:\\", t"\\", Case.Upper):
  type Platform = Dos
