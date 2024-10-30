package turbulence

import scala.reflect.*

import anticipation.*
import denominative.*
import rudiments.*
import vacuous.*
import hypotenuse.*

object Conduit:
  enum State:
    case Data, Clutch, End

class Conduit(input: LazyList[Bytes]):
  private var current: Bytes = if input.isEmpty then Bytes() else input.head
  private var stream: LazyList[Bytes] = if input.isEmpty then LazyList() else input.tail
  private var index: Ordinal = Prim
  private var done: Int = 0
  private var clutch: Boolean = false

  private var stream0: LazyList[Bytes] = stream
  private var current0: Bytes = current
  private var index0: Ordinal = index
  private var done0: Int = done
  private var clutch0: Boolean = clutch

  def block: Bytes = current
  def datum: Byte = current(index.n0)

  def next(): Boolean = step() match
    case Conduit.State.Clutch => cue() yet next()
    case state                => state != Conduit.State.Clutch

  def break(): Unit = if !clutch then
    val prefix = current.slice(0, index.n1)
    val suffix = current.drop(index.n1)
    clutch = true
    current = prefix
    val stream0 = stream
    stream = suffix #:: stream0

  def breakBefore(): Unit = if !clutch then
    val prefix = current.slice(0, index.n0)
    val suffix = current.drop(index.n0)
    clutch = true
    current = prefix
    val stream0 = stream
    stream = suffix #:: stream0

  def save(): Bytes =
    val length = (done + index) - (done0 + index0)
    IArray.create(length): array =>
      var sourceIndex = index0.n0
      var destinationIndex = 0
      var head = current0
      var tail = stream0

      def recur(): Unit =
        val count = (head.length - sourceIndex).min(array.length - destinationIndex)
        System.arraycopy(head.mutable(using Unsafe), sourceIndex, array, destinationIndex, count)
        destinationIndex += count
        if destinationIndex < array.length then
          head = tail.head
          tail = tail.tail
          sourceIndex = 0
          recur()

      recur()

  @tailrec
  final def seek(byte: Byte): Unit = if next() && datum != byte then seek(byte)

  @tailrec
  final def skip(count: Int): Unit = if count > 0 then next() yet skip(count - 1)

  def step(): Conduit.State =
    if clutch then
      if stream.isEmpty then Conduit.State.End else
        clutch = false
        step()
    else
      index += 1
      if index > current.ult.or(Prim - 1) then
        clutch = true
        Conduit.State.Clutch
      else Conduit.State.Data

  def cue(): Unit =
    if !stream.isEmpty then
      done += current.length
      current = stream.head
      val tail = stream.tail
      stream = tail
      index = Prim - 1
      clutch = false

  def mark(): Unit =
    current0 = current
    index0 = index
    done0 = done
    clutch0 = clutch

  def revert(): Unit =
    current = current0
    index = index0
    done = done0
    clutch = clutch0

  inline def lookahead[ResultType](inline action: => ResultType): ResultType =
    mark()
    try action finally revert()
