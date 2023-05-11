package gossamer

import rudiments.*

import language.experimental.captureChecking

erased trait DefaultTextType:
  type TextType

package defaultTextTypes:
  given text: DefaultTextType { type TextType = Text } = compiletime.erasedValue

trait Textual[TextType]:
  def length(text: TextType): Int
  def string(text: TextType): String
  def make(string: String): TextType
  def map(text: TextType, fn: Char => Char): TextType
  def slice(text: TextType, start: Int, end: Int): TextType
  def empty: TextType
  def concat(left: TextType, right: TextType): TextType
  def unsafeChar(text: TextType, index: Int): Char
  def indexOf(text: TextType, sub: Text): Int

object Textual:
  given Textual[Text] with
    def string(text: Text): String = text.s
    def length(text: Text): Int = text.s.length
    def make(string: String): Text = Text(string)
    def map(text: Text, fn: Char => Char): Text = Text(text.s.map(fn))
    def slice(text: Text, start: Int, end: Int): Text = Text(text.s.substring(start, end).nn)
    def empty: Text = Text("")
    def concat(left: Text, right: Text): Text = Text(left.s+right.s)
    def unsafeChar(text: Text, index: Int): Char = text.s.charAt(index)
    def indexOf(text: Text, sub: Text): Int = text.s.indexOf(sub.s)

  // given Textual[String] with
  //   def string(string: String): String = string
  //   def length(string: String): Int = string.length
  //   def make(string: String): String = string
  //   def map(string: String, fn: Char -> Char): String = string.map(fn)
  //   def slice(string: String, start: Int, end: Int): String = string.substring(start, end).nn
  //   def empty: String = ""
  //   def concat(left: String, right: String): String = left+right
  //   def unsafeChar(string: String, index: Int): Char = string.charAt(index)
  //   def indexOf(string: String, sub: Text): Int = string.indexOf(sub.s)
