package yossarian

import rudiments.*
import iridescence.*

object Yossarian:
  // Bit packing
  //  0-23 Foreground color
  // 24-47 Background color
  //    48 Bold
  //    49 Italic
  //    50 Strike
  opaque type Style = Long
  opaque type ScreenBuffer = (Int, Array[Style], Array[Char])

  extension (buffer: ScreenBuffer)
    def offset(x: Int, y: Int): Int = y*buffer(0) + x
    def style(x: Int, y: Int): Style = buffer(1)(offset(x, y))
    def char(x: Int, y: Int): Style = buffer(2)(offset(x, y))
    def width: Int = buffer(0)
    def height: Int = buffer(1).length/buffer(0)
    def size: Int = buffer(1).length

    def scroll(n: Int): Unit =
      val start = width*n
      val end = size - width*n
      System.arraycopy(buffer(1), start.max(0), buffer(1), (-start).max(0), end)
      System.arraycopy(buffer(2), start.max(0), buffer(2), (-start).max(0), end)
      
      for i <- 0 until -width*n do
        buffer(1)(i) = Style()
        buffer(2)(i) = ' '
    
    def set(x: Int, y: Int, char: Char, style: Style): Unit =
      buffer(1)(offset(x, y)) = style
      buffer(2)(offset(x, y)) = char

    def copy(): ScreenBuffer =
      val style = new Array[Style](buffer(0))
      System.arraycopy(buffer(1), 0, style, 0, style.length)
      val chars = new Array[Char](buffer(0))
      System.arraycopy(buffer(2), 0, chars, 0, chars.length)
      (buffer(0), style, chars)

  object ScreenBuffer:
    def apply(width: Int, height: Int): ScreenBuffer =
      val chars = Array.fill[Char](width*height)(' ')
      val style = Array.fill[Style](width*height)(Style())
      (width, style, chars)

  extension (style: Style)
    def foreground: Rgb24 = Rgb24(((style >> 40) & 0xffffff).toInt)
    def foreground_=(color: Rgb24): Style = (color.asRgb24Int << 40) + (style & 0x000000ffffffffffL)

    def background: Rgb24 = Rgb24(((style >> 16) & 0xffffff).toInt)
    def background_=(color: Rgb24): Style = (color.asRgb24Int << 16) + (style & 0xffffff000000ffffL)
    
    def bold: Boolean = ((style >> 15) & 1L) == 1L
    def bold_=(state: Boolean): Style = if state then (style | (1L << 15)) else (style & (1L << 15))
    
    def faint: Boolean = ((style >> 14) & 1L) == 1L
    def faint_=(state: Boolean): Style = if state then (style | (1L << 14)) else (style & (1L << 14))
    
    def italic: Boolean = ((style >> 13) & 1L) == 1L
    def italic_=(state: Boolean): Style = if state then (style | (1L << 13)) else (style & (1L << 13))
    
    def strike: Boolean = ((style >> 12) & 1L) == 1L
    def strike_=(state: Boolean): Style = if state then (style | (1L << 12)) else (style & (1L << 12))
    
    def blink: Boolean = ((style >> 11) & 1L) == 1L
    def blink_=(state: Boolean): Style = if state then (style | (1L << 11)) else (style & (1L << 11))
    
    def underline: Boolean = ((style >> 10) & 1L) == 1L
    def underline_=(state: Boolean): Style = if state then (style | (1L << 10)) else (style & (1L << 10))
    
    def conceal: Boolean = ((style >> 9) & 1L) == 1L
    def conceal_=(state: Boolean): Style = if state then (style | (1L << 9)) else (style & (1L << 9))
    
    def reverse: Boolean = ((style >> 8) & 1L) == 1L
    def reverse_=(state: Boolean): Style = if state then (style | (1L << 8)) else (style & (1L << 8))

  object Style:
    def apply(): Style = 0L.foreground = Rgb24(255, 255, 255)

export Yossarian.*