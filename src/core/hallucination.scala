package hallucination

import escapade.*
import turbulence.*
import rudiments.*
import anticipation.*
import iridescence.*

import java.awt.image as jai
import java.awt as ja
import javax.imageio as ji

enum ImageFormat:
  case Png, Jpeg, Gif, Bmp, Tiff

case class Image(private[hallucination] val image: jai.BufferedImage):
  def width: Int = image.getWidth
  def height: Int = image.getHeight
  def apply(x: Int, y: Int): Rgb24 =
    val color: ja.Color = ja.Color(image.getRGB(x, y), true)
    Rgb24(color.getRed, color.getGreen, color.getBlue)

  def render: Text =
    val buffer = new StringBuilder()
    for y <- (height - 1) until 0 by -2 do
      for x <- 0 until width do
        val foreground = apply(x, y)
        val background = apply(x, y - 1)
        buffer.append(e"${apply(x, y)}(${Bg(apply(x, y - 1))}(▀))".render)
      
      buffer.append('\n')
    
    buffer.toString.tt

object Image:
  def read[InputType](inputType: InputType)(using Readable[InputType, Bytes]): Image =
    Image(ji.ImageIO.read(inputType.readAs[Bytes].javaInputStream).nn)
