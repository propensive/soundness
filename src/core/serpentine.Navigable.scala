package serpentine

import anticipation.*
import prepositional.*
import nomenclature.*

trait Navigable extends Nominative:
  type Self
  type Operand
  type Source <: Root on Self

  def rootLength(path: Text): Int
  def root(path: Text): Source
  def rootText(root: Source): Text
  def element(element: Text): Operand
  def elementText(element: Operand): Text
  def separator: Text
  def selfText: Text
  def parentElement: Text
  def ascent: Text = parentElement+separator
  //def caseSensitive: Boolean
