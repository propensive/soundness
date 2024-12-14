package serpentine

import anticipation.*
import nomenclature.*
import prepositional.*

trait Navigable extends Nominative:
  type Self
  type Operand

  def element(element: Text): Operand
  protected def elementText(element: Operand): Text
  def separator: Text
  def selfText: Text
  def parentElement: Text
  def ascent: Text = parentElement+separator
  def caseSensitivity: Case
  def makeElement(element: Operand): Text = caseSensitivity(elementText(element))
