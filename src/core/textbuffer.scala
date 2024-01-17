package gossamer

import rudiments.*
import anticipation.*
import spectacular.*

def append
    [TextType, ValueType]
    (using buffer: Buffer[TextType])
    (using textual: Textual[TextType])
    (value: ValueType)
    (using show: textual.ShowType[ValueType]) =
  buffer.append(textual.show(value))

extension (textObject: Text.type)
  def make(block: (buffer: TextBuffer) ?=> Unit): Text =
    val buffer = TextBuffer()
    block(using buffer)
    buffer()

abstract class Buffer[TextType]():
  protected def put(text: TextType): Unit
  protected def wipe(): Unit
  protected def result(): TextType
  
  def append(text: TextType): this.type = this.also(put(text))
  def apply(): TextType = result()
  def clear(): this.type = this.also(wipe())

class TextBuffer() extends Buffer[Text]():
  private val buffer: StringBuilder = StringBuilder()
  protected def put(text: Text): Unit = buffer.append(text)
  protected def wipe(): Unit = buffer.clear()
  protected def result(): Text = buffer.toString().tt