                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package hallucination

import anticipation.*
import contingency.*
import fulminate.*
import vacuous.*
import proscenium.compat.*

// The pure-Scala backend, selected on Scala.js and WASI. The codecs themselves live in `core`
// (so they compile, and are differentially tested against `javax.imageio`, on the JVM); this
// object only dispatches.
private[hallucination] object RasterBackend:
  def decode(format: Rasterizable, data: Data): Raster raises RasterError = format.name.s match
    case "PNG"  => PngCodec.decode(data)
    case "BMP"  => BmpCodec.decode(data)
    case "GIF"  => GifCodec.decode(data)
    case "WEBP" => WebpCodec.decode(data)
    case "JPEG" => JpegCodec.decode(data)
    case _      => abort(RasterError(format))

  // Format-agnostic decoding, recognising the format by its opening magic bytes.
  def decode(data: Data): Raster raises RasterError =
    if data.length < 4 then abort(RasterError(Unset, RasterError.Reason.Truncated))
    else if (data(0)&0xff) == 0x89 && data(1) == 0x50 then PngCodec.decode(data)
    else if data(0) == 0x47 && data(1) == 0x49 && data(2) == 0x46 then GifCodec.decode(data)
    else if data(0) == 0x42 && data(1) == 0x4d then BmpCodec.decode(data)
    else if JpegCodec.isJpeg(data) then JpegCodec.decode(data)
    else if WebpCodec.isWebp(data) then WebpCodec.decode(data)
    else abort(RasterError(Unset))

  def encode(format: Rasterizable, raster: Raster): Data = format.name.s match
    case "PNG"  => PngCodec.encode(raster)
    case "BMP"  => BmpCodec.encode(raster)
    case "GIF"  => GifCodec.encode(raster)
    case "WEBP" => WebpCodec.encode(raster)
    case "JPEG" => JpegEncoder.encode(raster)
    case _      => panic(m"the ${format.name} format has no native encoder")
