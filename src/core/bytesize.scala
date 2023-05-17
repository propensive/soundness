package rudiments

import anticipation.*

import language.experimental.captureChecking

extension (bs: Int)
  def b: ByteSize = bs
  def kb: ByteSize = bs*1024L
  def mb: ByteSize = bs*1024L*1024
  def gb: ByteSize = bs*1024L*1024*1024
  def tb: ByteSize = bs*1024L*1024*1024*1024

extension (bs: Long)
  def b: ByteSize = bs
  def kb: ByteSize = bs*1024
  def mb: ByteSize = bs*1024*1024
  def gb: ByteSize = bs*1024*1024*1024
  def tb: ByteSize = bs*1024*1024*1024*1024

opaque type ByteSize = Long

object ByteSize:
  def apply(long: Long): ByteSize = long
  given GenericHttpRequestParam["content-length", ByteSize] = _.long.toString
  given Ordering[ByteSize] = Ordering.Long.on(_.long)

  extension (bs: ByteSize)
    def long: Long = bs
    def text: Text = Text(bs.toString+" bytes")

    @targetName("plus")
    infix def +(that: ByteSize): ByteSize = bs + that

    @targetName("gt")
    infix def >(that: ByteSize): Boolean = bs > that

    @targetName("lt")
    infix def <(that: ByteSize): Boolean = bs < that

    @targetName("lte")
    infix def <=(that: ByteSize): Boolean = bs <= that

    @targetName("gte")
    infix def >=(that: ByteSize): Boolean = bs >= that

    @targetName("minus")
    infix def -(that: ByteSize): ByteSize = bs - that

    @targetName("times")
    infix def *(that: Int): ByteSize = bs*that

    @targetName("div")
    infix def /(that: Int): ByteSize = bs/that
