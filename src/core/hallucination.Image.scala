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

import anticipation.*
import escapade.*
import gossamer.*
import iridescence.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*

import java.awt.image as jai
import java.awt as ja
import javax.imageio as ji

open case class Image(private[hallucination] val image: jai.BufferedImage):
  type Format <: ImageFormat
  def width: Int = image.getWidth
  def height: Int = image.getHeight

  def apply(x: Int, y: Int): Rgb24 =
    val color: ja.Color = ja.Color(image.getRGB(x, y), true)
    Rgb24(color.getRed, color.getGreen, color.getBlue)

  def rasterize(using termcap: Termcap): Text = Text.construct:
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
