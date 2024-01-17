/*
    Hallucination, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hallucination

import escapade.*
import turbulence.*
import rudiments.*
import anticipation.*
import spectacular.*
import iridescence.*
import gossamer.*

import java.awt.image as jai
import java.awt as ja
import javax.imageio as ji

class ImageCodec[ImageFormatType <: ImageFormat](name: Text):
  protected lazy val reader: ji.ImageReader = ji.ImageIO.getImageReaders(name.s).nn.next().nn
  protected lazy val writer: ji.ImageWriter = ji.ImageIO.getImageWriter(reader).nn
  
  def read[InputType](inputType: InputType)(using Readable[InputType, Bytes]): Image[?] =
    reader.synchronized:
      reader.setInput(ji.ImageIO.createImageInputStream(inputType.readAs[Bytes].javaInputStream).nn)
      Image(reader.read(0).nn).also(reader.dispose())

erased trait ImageFormat
erased trait Bmp extends ImageFormat
erased trait Jpeg extends ImageFormat
erased trait Gif extends ImageFormat
erased trait Png extends ImageFormat

object Jpeg extends ImageCodec[Jpeg]("JPEG".tt)
object Bmp extends ImageCodec[Bmp]("BMP".tt)
object Gif extends ImageCodec[Gif]("GIF".tt)
object Png extends ImageCodec[Png]("PNG".tt)

case class Image[ImageFormatType <: ImageFormat](private[hallucination] val image: jai.BufferedImage):
  def width: Int = image.getWidth
  def height: Int = image.getHeight
  
  def apply(x: Int, y: Int): Rgb24 =
    val color: ja.Color = ja.Color(image.getRGB(x, y), true)
    Rgb24(color.getRed, color.getGreen, color.getBlue)

  def render: Text = Text.make:
    for y <- 0 until (height - 1) by 2 do
      for x <- 0 until width do append(e"${apply(x, y)}(${Bg(apply(x, y + 1))}(▀))".render)
      append('\n')

object Image:
  def apply[InputType](inputType: InputType)(using Readable[InputType, Bytes]): Image[?] =
    Image(ji.ImageIO.read(inputType.readAs[Bytes].javaInputStream).nn)
