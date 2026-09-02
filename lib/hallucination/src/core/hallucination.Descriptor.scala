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

import scala.compiletime.*

import anticipation.*
import iridescence.*
import rudiments.*
import vacuous.*

// The runtime mirror of a pixel layout: one entry per channel, most significant first. It is
// fixed at construction from the static layout, and powers the layout-agnostic operations —
// `Graphical`, `Raster`'s untyped `apply`, repacking and the codec backends — which cannot use
// `Pixel`'s inline accessors.
//
// `@unexported`: `Descriptor` would clash with embarcadero's OCI `Descriptor` in the
// `soundness` umbrella; reach it via `hallucination.Descriptor`.
@unexported
object Descriptor:
  case class Entry(label: Text, depth: Int)

  val rgb: Descriptor = Descriptor.of[Rgb]
  val rgba: Descriptor = Descriptor.of[Rgba]

  inline def of[layout <: Tuple]: Descriptor = Descriptor(entries[layout])

  private inline def entries[layout <: Tuple]: List[Entry] =
    inline erasedValue[layout] match
      case _: EmptyTuple => Nil

      case _: (head *: tail) =>
        Entry(constValue[Channel.Label[head]].tt, constValue[Channel.Bits[head]]) ::
          entries[tail]

case class Descriptor(entries: List[Descriptor.Entry]):
  val totalBits: Int =
    val depths: List[Int] = entries.map(_.depth)
    depths.total

  // The width of the storage primitive for one pixel, mirroring `Channel.Storage`.
  def storageBits: Int =
    if totalBits <= 8 then 8 else if totalBits <= 16 then 16 else if totalBits <= 32 then 32 else 64

  // The shift and depth of the labelled channel, if the layout has it.
  def locate(label: Text): Optional[(Int, Int)] =
    def recur(entries: List[Descriptor.Entry], shift: Int): Optional[(Int, Int)] = entries match
      case Nil => Unset

      case entry :: tail =>
        val shift2 = shift - entry.depth
        if entry.label == label then (shift2, entry.depth) else recur(tail, shift2)

    recur(entries, totalBits)

  def has(label: Text): Boolean = entries.exists(_.label == label)
  def hasAlpha: Boolean = has("alpha".tt)

  // The channel positions of each colour model the layout might carry, resolved once per
  // descriptor: present exactly when *every* channel of the group is present. The per-pixel
  // operations below branch on these rather than on `has`, so a malformed layout (e.g. `red`
  // without `green`) degrades to the next fallback path instead of panicking, and the
  // per-pixel bodies perform no channel lookups and no allocation.
  private lazy val rgbPositions: Optional[((Int, Int), (Int, Int), (Int, Int))] =
    locate("red".tt).let: red =>
      locate("green".tt).let: green =>
        locate("blue".tt).let: blue =>
          (red, green, blue)

  private lazy val cmykPositions: Optional[((Int, Int), (Int, Int), (Int, Int), (Int, Int))] =
    locate("cyan".tt).let: cyan =>
      locate("magenta".tt).let: magenta =>
        locate("yellow".tt).let: yellow =>
          locate("key".tt).let: key =>
            (cyan, magenta, yellow, key)

  private lazy val alphaPosition: Optional[(Int, Int)] = locate("alpha".tt)
  private lazy val greyPosition: Optional[(Int, Int)] = locate("grey".tt)

  private def component(word: Long, position: (Int, Int)): Int =
    val (shift, depth) = position
    ((word >>> shift)&((1L << depth) - 1)).toInt

  private def proportion(word: Long, position: (Int, Int)): Double =
    component(word, position).toDouble/((1 << position(1)) - 1)

  // The opacity of the pixel in the unit interval: fully opaque if the layout has no alpha
  // channel.
  def alpha(word: Long): Double =
    alphaPosition.lay(1.0)(proportion(word, _))

  // The colour of the pixel as `Srgb`, the runtime counterpart of `Pixel`'s `srgb`. Tries
  // RGB, then CMYK, then grey: an incomplete channel group falls through to the next model.
  def srgb(word: Long): Srgb =
    def grey: Srgb =
      val luma = greyPosition.lay(0.0)(proportion(word, _))
      Srgb(luma, luma, luma)

    def cmyk: Srgb = cmykPositions.lay(grey): (cyan, magenta, yellow, key) =>
      Cmyk
        ( proportion(word, cyan),
          proportion(word, magenta),
          proportion(word, yellow),
          proportion(word, key) )

      . to[Srgb]

    rgbPositions.lay(cmyk): (red, green, blue) =>
      Srgb(proportion(word, red), proportion(word, green), proportion(word, blue))

  // Rescales the pixel to 24-bit RGB with the same rounding as `Pixel`'s `chroma`.
  def chroma(word: Long): Chroma =
    def fromSrgb: Chroma =
      val color = srgb(word)

      Chroma
        ( (color.red*255 + 0.5).toInt,
          (color.green*255 + 0.5).toInt,
          (color.blue*255 + 0.5).toInt )

    rgbPositions.lay(fromSrgb): (red, green, blue) =>
      def rescale(position: (Int, Int)): Int =
        val maximum = (1 << position(1)) - 1
        (component(word, position)*255 + maximum/2)/maximum

      Chroma(rescale(red), rescale(green), rescale(blue))

  // Packs a colour and an opacity into a pixel, the runtime counterpart of `Pixel`'s `apply`.
  def pack(color: Srgb, alpha: Double = 1.0): Long =
    def scale(proportion: Double, position: (Int, Int)): Long =
      val (shift, depth) = position
      ((proportion*((1L << depth) - 1) + 0.5).toLong) << shift

    def grey: Long =
      greyPosition.lay(0L): position =>
        scale(color.red*0.299 + color.green*0.587 + color.blue*0.114, position)

    def cmyk: Long = cmykPositions.lay(grey): (cyanPos, magentaPos, yellowPos, keyPos) =>
      val key = 1.0 - color.red.max(color.green).max(color.blue)
      val white = 1.0 - key
      val cyan = if white == 0.0 then 0.0 else (white - color.red)/white
      val magenta = if white == 0.0 then 0.0 else (white - color.green)/white
      val yellow = if white == 0.0 then 0.0 else (white - color.blue)/white

      scale(cyan, cyanPos) + scale(magenta, magentaPos) + scale(yellow, yellowPos) +
        scale(key, keyPos)

    rgbPositions.lay(cmyk): (red, green, blue) =>
      val opaque = scale(color.red, red) + scale(color.green, green) + scale(color.blue, blue)
      alphaPosition.lay(opaque): position => opaque + scale(alpha, position)
