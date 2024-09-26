package serpentine

import anticipation.*
import prepositional.*
import nomenclature.*

trait Navigable extends Nominative:
  type Self
  type Operand

  def prefixLength(path: Text): Int
  def prefix(path: Text): Root on Self
  def rootText(root: Root on Self): Text
  def element(element: Text): Operand
  def elementText(element: Operand): Text
  def separator: Text
  def selfText: Text
  def parentElement: Text
  def ascent: Text = parentElement+separator
  //def caseSensitive: Boolean
