package gossamer

import rudiments.*
import anticipation.*

abstract class Buffer[TextType]():
  protected val textual: Textual[TextType]
  protected def append(text: TextType): Unit
  protected def wipe(): Unit
  protected def result(): TextType
  
  @targetName("append")
  def += [ValueType: textual.ShowType](value: ValueType): this.type =
    this.also(append(textual.show(value)))
  
  def apply(): TextType = result()
  def clear(): this.type = this.also(wipe())

case class TextBuffer() extends Buffer[Text]():
  private val buffer: StringBuilder = StringBuilder()
  protected val textual: Textual[Text] = Textual.text
  protected def append(text: Text): Unit = buffer.append(text)
  protected def wipe(): Unit = buffer.clear()
  protected def result(): Text = buffer.toString().tt