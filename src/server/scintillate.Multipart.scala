package scintillate

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import rudiments.*
import turbulence.*

case class MultipartError()(using Diagnostics)
extends Error(m"The multipart data could not be read")

object Multipart:
  private def check
      (boundary: Bytes, current: Bytes, stream: LazyList[Bytes], index: Int, matched: Int)
          : Boolean =
    if matched >= boundary.length then true else
      if index < current.length
      then current(index) == boundary(matched) && check(boundary, current, stream, index + 1, matched + 1)
      else stream.flow(false)(check(boundary, head, tail, index - current.length, matched))

  def parts(boundary: Bytes, current: Bytes, stream: LazyList[Bytes], start: Int, index: Int)
          : LazyList[Part] raises MultipartError =
    val part: Part = Part(parsePart(boundary, current, stream, start, index))
    part #:: skip(boundary, part.length + boundary.length, current #:: stream, index)

  @tailrec
  def skip(boundary: Bytes, count: Int, stream: LazyList[Bytes], index: Int): LazyList[Part] raises MultipartError =
    stream match
      case head #:: tail =>
        if head.length < count then skip(boundary, count - head.length, tail, 0)
        else parts(boundary, head, tail, count, count)

      case _ =>
        throw Panic(m"This should never be possible")

  def parsePart(boundary: Bytes, current: Bytes, stream: LazyList[Bytes], start: Int, index: Int)
          : LazyList[Bytes] raises MultipartError =
    stream.flow(abort(MultipartError())):
      if index < current.length then current(index) match
        case '\r' if check(boundary, head, tail, index, 0) =>
          val range = Ordinal.zerary(start) ~ Ordinal.zerary(index)
          LazyList(current.segment(range))

        case _ =>
          parsePart(boundary, current, stream, start, index + 1)

      else
        val range = Ordinal.zerary(start) ~ Ordinal.zerary(index)
        current.segment(range) #:: parsePart(boundary, stream.head, stream.tail, 0, 0)

  def parse(boundary: Text, stream: LazyList[Bytes]): Multipart raises MultipartError =
    Multipart(parts(t"\r\n$boundary".sysBytes, stream.head, stream.tail, 0, 0))

case class Multipart(parts: LazyList[Part])

case class Part(body: LazyList[Bytes]):
  def length: Int = body.sumBy(_.length)
