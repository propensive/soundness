package serpentine

import anticipation.*
import prepositional.*

trait Navigable:
  type Self <: AnyRef & Matchable
  type Operand

  def prefixLength(path: Text): Int
  def prefix(path: Text): Self
  def rootText(root: Self): Text
  def element(element: Text): Operand
  def elementText(element: Operand): Text
  def separator: Text
  def selfText: Text
  def parentElement: Text
  def ascent: Text = parentElement+separator
  //def caseSensitive: Boolean
