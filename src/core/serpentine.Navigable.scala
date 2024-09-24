package serpentine

import anticipation.*
import prepositional.*
import gossamer.*

object Navigable:
  given Navigable by Nothing = new Navigable:
    type Operand = Nothing
    def element(text: Text): Nothing = ???
    def elementText(element: Nothing): anticipation.Text = t""
    def parentElement: Text = t".."
    def prefix(path: Text): Self = ???
    def prefixLength(path: Text): Int = 0
    def rootText(root: Self): Text = t""
    def separator: Text = t"/"
    def selfText: Text = t"."

trait Navigable:
  type Self <: AnyRef
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
