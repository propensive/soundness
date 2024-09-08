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
import prepositional.*
import anticipation.*
import gesticulate.*
import spectacular.*
import iridescence.*
import gossamer.*

import java.awt.image as jai
import java.awt as ja
import javax.imageio as ji

abstract class ImageCodec[ImageFormatType <: ImageFormat](name: Text):
  inline def codec: ImageCodec[ImageFormatType] = this
  protected def mediaType: MediaType
  protected lazy val reader: ji.ImageReader = ji.ImageIO.getImageReaders(name.s).nn.next().nn
  protected lazy val writer: ji.ImageWriter = ji.ImageIO.getImageWriter(reader).nn

  given (Image in ImageFormatType) is GenericHttpResponseStream as response:
    def mediaType = mediaType.show
    def content(image: Image in ImageFormatType): LazyList[Bytes] = image.serialize(using codec)

  def read[InputType: Readable by Bytes](inputType: InputType): Image in ImageFormatType =
    reader.synchronized:
      reader.setInput(ji.ImageIO.createImageInputStream(inputType.read[Bytes].javaInputStream).nn)

      (new Image(reader.read(0).nn) { type Format = ImageFormatType }).also:
        reader.dispose()

erased trait ImageFormat
erased trait Bmp extends ImageFormat
erased trait Jpeg extends ImageFormat
erased trait Gif extends ImageFormat
erased trait Png extends ImageFormat

object Jpeg extends ImageCodec[Jpeg]("JPEG".tt):
  def mediaType = media"image/jpeg"

object Bmp extends ImageCodec[Bmp]("BMP".tt):
  def mediaType = media"image/bmp"

object Gif extends ImageCodec[Gif]("GIF".tt):
  def mediaType = media"image/gif"

object Png extends ImageCodec[Png]("PNG".tt):
  def mediaType = media"image/png"

case class Image(private[hallucination] val image: jai.BufferedImage):
  type Format <: ImageFormat
  def width: Int = image.getWidth
  def height: Int = image.getHeight

  def apply(x: Int, y: Int): Rgb24 =
    val color: ja.Color = ja.Color(image.getRGB(x, y), true)
    Rgb24(color.getRed, color.getGreen, color.getBlue)

  def render(using termcap: Termcap): Text = Text.construct:
    for y <- 0 until (height - 1) by 2 do
      for x <- 0 until width do append(e"${apply(x, y)}(${Bg(apply(x, y + 1))}(▀))".render(termcap))
      append('\n')

  def serialize(using codec: ImageCodec[Format]): LazyList[Bytes] =
    val out = LazyListOutputStream()
    ji.ImageIO.createImageOutputStream(out)
    out.stream

object Image:
  def apply[InputType: Readable by Bytes](inputType: InputType): Image =
    Image(ji.ImageIO.read(inputType.read[Bytes].javaInputStream).nn)
