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
