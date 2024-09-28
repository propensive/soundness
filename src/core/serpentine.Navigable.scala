package serpentine

import anticipation.*
import prepositional.*
import nomenclature.*

trait Navigable extends Nominative:
  type Self
  type Operand

  def rootLength(path: Text): Int
  def root(path: Text): Root on Self
  def rootText(root: Root on Self): Text
  def element(element: Text): Operand
  def elementText(element: Operand): Text
  def separator: Text
  def selfText: Text
  def parentElement: Text
  def ascent: Text = parentElement+separator
  //def caseSensitive: Boolean
