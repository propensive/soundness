package escapade

import gossamer.*
import vacuous.*
import rudiments.*
import anticipation.*

import scala.collection.mutable as scm

class TeletypeBuffer(size: Optional[Int]) extends Buffer[Teletype]:
  private val buffer: StringBuilder = StringBuilder()
  private val spans: scm.Map[CharSpan, Ansi.Transform] = scm.HashMap()
  private val insertions: scm.Map[Int, Text] = scm.HashMap()
  private var offset: Int = 0

  protected def wipe(): Unit =
    offset = 0
    buffer.clear()
    spans.clear()
    insertions.clear()

  protected def put(text: Teletype): Unit =
    buffer.append(text.plain.s)
    text.spans.each { (span, value) => spans(span.shift(offset)) = value }
    text.insertions.each { (position, value) => insertions(position + offset) = value }
    offset += text.length

  protected def result(): Teletype =
    Teletype(buffer.toString.tt, spans.to(TreeMap), insertions.to(TreeMap))
