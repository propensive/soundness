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
  val totalBits: Int = entries.stdlib.sumBy(_.depth)

  // The width of the storage primitive for one pixel, mirroring `Channel.Storage`.
  def storageBits: Int =
    if totalBits <= 8 then 8 else if totalBits <= 16 then 16 else if totalBits <= 32 then 32
    else 64

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

  private def component(word: Long, position: (Int, Int)): Int =
    val (shift, depth) = position
    ((word >>> shift)&((1L << depth) - 1)).toInt

  private def proportion(word: Long, position: (Int, Int)): Double =
    component(word, position).toDouble/((1 << position(1)) - 1)

  // The opacity of the pixel in the unit interval: fully opaque if the layout has no alpha
  // channel.
  def alpha(word: Long): Double =
    locate("alpha".tt).lay(1.0)(proportion(word, _))

  // The colour of the pixel as `Srgb`, the runtime counterpart of `Pixel`'s `srgb`.
  def srgb(word: Long): Srgb =
    if has("red".tt) then
      Srgb
        ( proportion(word, locate("red".tt).vouch),
          proportion(word, locate("green".tt).vouch),
          proportion(word, locate("blue".tt).vouch) )
    else if has("cyan".tt) then
      Cmyk
        ( proportion(word, locate("cyan".tt).vouch),
          proportion(word, locate("magenta".tt).vouch),
          proportion(word, locate("yellow".tt).vouch),
          proportion(word, locate("key".tt).vouch) )

      . to[Srgb]
    else
      val luma = locate("grey".tt).lay(0.0)(proportion(word, _))
      Srgb(luma, luma, luma)

  // Rescales the pixel to 24-bit RGB with the same rounding as `Pixel`'s `chroma`.
  def chroma(word: Long): Chroma =
    if has("red".tt) then
      def rescale(label: Text): Int =
        val position = locate(label).vouch
        val maximum = (1 << position(1)) - 1
        (component(word, position)*255 + maximum/2)/maximum

      Chroma(rescale("red".tt), rescale("green".tt), rescale("blue".tt))
    else
      val color = srgb(word)

      Chroma
        ( (color.red*255 + 0.5).toInt,
          (color.green*255 + 0.5).toInt,
          (color.blue*255 + 0.5).toInt )

  // Packs a colour and an opacity into a pixel, the runtime counterpart of `Pixel`'s `apply`.
  def pack(color: Srgb, alpha: Double = 1.0): Long =
    def scale(proportion: Double, position: (Int, Int)): Long =
      val (shift, depth) = position
      ((proportion*((1L << depth) - 1) + 0.5).toLong) << shift

    if has("red".tt) then
      val opaque =
        scale(color.red, locate("red".tt).vouch) +
          scale(color.green, locate("green".tt).vouch) +
          scale(color.blue, locate("blue".tt).vouch)

      locate("alpha".tt).lay(opaque): position =>
        opaque + scale(alpha, position)

    else if has("cyan".tt) then
      val key = 1.0 - color.red.max(color.green).max(color.blue)
      val white = 1.0 - key
      val cyan = if white == 0.0 then 0.0 else (white - color.red)/white
      val magenta = if white == 0.0 then 0.0 else (white - color.green)/white
      val yellow = if white == 0.0 then 0.0 else (white - color.blue)/white

      scale(cyan, locate("cyan".tt).vouch) +
        scale(magenta, locate("magenta".tt).vouch) +
        scale(yellow, locate("yellow".tt).vouch) +
        scale(key, locate("key".tt).vouch)

    else
      locate("grey".tt).lay(0L): position =>
        scale(color.red*0.299 + color.green*0.587 + color.blue*0.114, position)
