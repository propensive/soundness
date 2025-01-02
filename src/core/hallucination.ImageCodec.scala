/*
    Hallucination, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import gesticulate.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*

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
