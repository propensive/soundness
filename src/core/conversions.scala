/*
    Iridescence, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package iridescence

import fulminate.*
import contextual.*
import hypotenuse.*
import anticipation.*
import rudiments.*

import language.experimental.captureChecking
import scala.util.chaining.*

private[iridescence] inline def unitary(d: Double): Double =
  d - d.toInt + (if d < 0 then 1 else 0)

object Xyz:
  given RgbColor[Xyz] = _.srgb.rgb24.asInt

case class Xyz(x: Double, y: Double, z: Double):
  def luminescence: Double = y
  
  def srgb: Srgb =
    def limit(v: Double): Double =
      if v > 0.0031308 then 1.055*(v**(1/2.4)) - 0.055 else 12.92*v
    
    val red = limit(x*0.032406994 - y*0.0153738318 - z*0.0049861076)
    val green = limit(-x*0.0096924364 + y*0.01878675 + z*0.0004155506)
    val blue = limit(x*0.0005563008 - y*0.0020397696 + z*0.0105697151)

    Srgb(red, green, blue)
  
  def rgb24: Rgb24 = srgb.rgb24
  
  def cielab(using profile: ColorProfile): Cielab =
    def limit(v: Double): Double = if v > 0.008856 then v**(1.0/3) else 7.787*v + 0.13793

    val l: Double = 116*limit(y/profile.y2) - 16
    val a: Double = 500*(limit(x/profile.x2) - limit(y/profile.y2))
    val b: Double = 200*(limit(y/profile.y2) - limit(z/profile.z2))

    Cielab(l, a, b)

object RgbHex extends Interpolator[Nothing, Option[Rgb24], Rgb24]:
  def initial: Option[Rgb24] = None
  def parse(state: Option[Rgb24], next: Text): Option[Rgb24] =
    if next.s.length == 7 && next.s.startsWith("#") then parse(state, Text(next.s.substring(1).nn))
    else if next.s.length == 6 && next.s.all: char =>
      char.isDigit || ((char | 32) >= 'a' && (char | 32) <= 'f')
    then
      val red = Integer.parseInt(next.s.substring(0, 2).nn, 16)
      val green = Integer.parseInt(next.s.substring(2, 4).nn, 16)
      val blue = Integer.parseInt(next.s.substring(4, 6).nn, 16)

      Some(Rgb24(red, green, blue))
    
    else throw InterpolationError(msg"""the color must be in the form ${Text("rgb\"#rrggbb\"")} or
        rgb"rrggbb" where rr, gg and bb are 2-digit hex values""", 0)
  
  def insert(state: Option[Rgb24], value: Nothing): Option[Rgb24] =
    throw InterpolationError(msg"substitutions into an ${Text("rgb\"\"")} interpolator are not supported")
  
  def skip(state: Option[Rgb24]): Option[Rgb24] = state
  def complete(color: Option[Rgb24]): Rgb24 = color.get

object Rgb24Opaque:
  opaque type Rgb24 = Int
  
  object Rgb24:
    given RgbColor[Rgb24] = _.asInt

    def apply(red: Int, green: Int, blue: Int): Rgb24 =
      ((red&255) << 16) + ((green&255) << 8) + (blue&255)

    def apply(packedInt: Int): Rgb24 = packedInt & 0x00ffffff
  
  extension (color: Rgb24)
    def red: Int = (color >> 16) & 255
    def green: Int = (color >> 8) & 255
    def blue: Int = color&255
  
    def srgb: Srgb = Srgb(red/255.0, green/255.0, blue/255.0)
    def asInt: Int = color
  
    def hex: Text = Text:
      List(red, green, blue).foldLeft("#"): (acc, c) =>
        acc+(c.hex.pipe { s => if s.s.length < 2 then "0"+s else s })

object Rgb32Opaque:
  opaque type Rgb32 = Int
  
  object Rgb32:
    given RgbColor[Rgb32] = _.srgb.rgb24.asInt

    def apply(red: Int, green: Int, blue: Int): Rgb32 =
      ((red&1023) << 22) + ((green&4095) << 10) + (blue&1023)
  
  extension (color: Rgb32)
    def red: Int = (color >> 22)&1023
    def green: Int = (color >> 10)&4095
    def blue: Int = color&1023
    def srgb: Srgb = Srgb(red/1023.0, green/4095.0, blue/1023.0)
  
object Rgb12Opaque:
  opaque type Rgb12 = Int
  
  object Rgb12:
    given RgbColor[Rgb12] = _.srgb.rgb24.asInt

    def apply(red: Int, green: Int, blue: Int): Rgb12 =
      ((red&15) << 8) + ((green&15) << 4) + (blue&15)
  
  extension (color: Rgb12)
    def red: Int = (color >> 8)&15
    def green: Int = (color >> 4)&15
    def blue: Int = color&15
    def hex: Text = Text("#"+List(red, green, blue).map(_.hex).mkString)
    def srgb: Srgb = Srgb(red/15.0, green/15.0, blue/15.0)

export Rgb12Opaque.Rgb12
export Rgb24Opaque.Rgb24
export Rgb32Opaque.Rgb32

object Srgb:
  given RgbColor[Srgb] = _.rgb24.asInt

case class Srgb(red: Double, green: Double, blue: Double):
  def css: Text = Text(s"rgb(${(red*255).toInt}, ${(green*255).toInt}, ${(blue*255).toInt})")
  
  def rgb24: Rgb24 = Rgb24((red*255).toInt, (green*255).toInt, (blue*255).toInt)
  def srgb: Srgb = this
  def highContrast: Srgb = if hsl.lightness >= 0.5 then Srgb(0, 0, 0) else Srgb(1, 1, 1)

  def xyz(using ColorProfile): Xyz =
    def limit(v: Double): Double = if v > 0.04045 then ((v + 0.055)/1.055)**2.4 else v/12.92

    val List(r, g, b) = List(red, green, blue).map(limit(_)*100)

    val x = r*0.4124 + g*0.3576 + b*0.1805
    val y = r*0.2126 + g*0.7152 + b*0.0722
    val z = r*0.0193 + g*0.1192 + b*0.9505

    Xyz(x, y, z)
  
  def cielab(using ColorProfile): Cielab = xyz.cielab
  def cmy: Cmy = Cmy(1 - red, 1 - green, 1 - blue)
  def cmyk: Cmyk = cmy.cmyk

  def hsl: Hsl =
    val min = red min green min blue
    val max = red max green max blue
    val delta = max - min
    val lightness = (max + min)/2

    if delta == 0 then Hsl(0, 0, lightness)
    else
      val saturation = if lightness < 0.5 then delta/(max + min) else delta/(2 - max - min)
      val dRed = ((max - red)/6 + delta/2)/delta
      val dGreen = ((max - green)/6 + delta/2)/delta
      val dBlue = ((max - blue)/6 + delta/2)/delta

      val hue =
        if max == red then dBlue - dGreen
        else if max == green then 1.0/3 + dRed - dBlue
        else 2.0/3 + dGreen - dRed

      Hsl(unitary(hue), saturation, lightness)

  def hsv: Hsv =
    val min = red min green min blue
    val value = red max green max blue
    val delta = value - min

    if delta == 0 then Hsv(0, 0, value)
    else
      val saturation = delta/value
      val dr = ((value - red)/6) + (delta/2)/delta
      val dg = ((value - green)/6) + (delta/2)/delta
      val db = ((value - blue)/6) + (delta/2)/delta
      
      val hue =
        if value == red then db - dg
        else if value == green then 1.0/3 + dr - db
        else 2.0/3 + dg - dr

      Hsv(unitary(hue), saturation, value)

object Cielab:
  given (using ColorProfile): RgbColor[Cielab] = _.srgb.rgb24.asInt

case class Cielab(l: Double, a: Double, b: Double):
  def srgb(using ColorProfile): Srgb = xyz.srgb

  def xyz(using profile: ColorProfile): Xyz =
    def limit(v: Double): Double = if v*v*v > 0.008856 then v*v*v else (v - 16.0/116)/7.787
  
    val y = limit((l + 16)/116)*profile.y2
    val x = limit(a/500 + (l + 16)/116)*profile.x2
    val z = limit((l + 16)/116 - b/200)*profile.z2

    Xyz(x, y, z)

  def mix(that: Cielab, ratio: Double = 0.5): Cielab =
    Cielab(l*(1 - ratio) + ratio*that.l, a*(1 - ratio) + ratio*that.a, b*(1 - ratio) + ratio*that.b)

  def delta(that: Cielab): Double = (hyp(that.a, that.b) - hyp(a, b)).double

object Cmy:
  given RgbColor[Cmy] = _.srgb.rgb24.asInt

case class Cmy(cyan: Double, magenta: Double, yellow: Double):
  def srgb: Srgb = Srgb((1 - cyan), (1 - magenta), (1 - yellow))
  
  def cmyk: Cmyk =
    val key = List(1, cyan, magenta, yellow).min
    
    if key == 1 then Cmyk(0, 0, 0, 1)
    else Cmyk((cyan - key)/(1 - key), (magenta - key)/(1 - key), (yellow - key)/(1 - key), key)

object Cmyk:
  given RgbColor[Cmyk] = _.srgb.rgb24.asInt

case class Cmyk(cyan: Double, magenta: Double, yellow: Double, key: Double):
  def srgb: Srgb = cmy.srgb
  def cmy: Cmy = Cmy(cyan*(1 - key) + key, magenta*(1 - key) + key, yellow*(1 - key) + key)

object Hsv:
  given RgbColor[Hsv] = _.srgb.rgb24.asInt

case class Hsv(hue: Double, saturation: Double, value: Double):
  def saturate: Hsv = Hsv(hue, 1, value)
  def desaturate: Hsv = Hsv(hue, 0, value)
  def rotate(degrees: Double): Hsv = Hsv(unitary(hue + degrees/360), saturation, value)
  def pure: Hsv = Hsv(hue, 1, 0)
  def tone(black: Double = 0, white: Double = 0) = shade(black).tint(white)
  def shade(black: Double = 0): Hsv = Hsv(hue, saturation, value*(1 - black) + (1 - value)*black)
  
  def srgb: Srgb =
    if saturation == 0 then Srgb(value, value, value)
    else
      val i = (hue*6).toInt%6
      val a1 = value*(1 - saturation)
      val a2 = value*(1 - saturation*(hue*6 - i))
      val a3 = value*(1 - saturation*(1 - (hue*6 - i)))

      val red = if i == 1 then a2 else if i/2 == 1 then a1 else if i == 4 then a3 else value
      val green = if i/2 == 2 then a1 else if i == 3 then a2 else if i == 0 then a3 else value
      val blue = if i/2 == 0 then a1 else if i == 2 then a3 else if i == 5 then a2 else value

      Srgb(red, green, blue)
 
  def tint(white: Double = 0): Hsv =
    Hsv(hue, saturation*(1 - white) + (1 - saturation)*white, value)
  
object Hsl:
  given RgbColor[Hsl] = _.srgb.rgb24.asInt

case class Hsl(hue: Double, saturation: Double, lightness: Double):
  def saturate: Hsv = Hsv(hue, 1, lightness)
  def desaturate: Hsv = Hsv(hue, 0, lightness)
  def rotate(degrees: Double): Hsv = Hsv(unitary(hue + degrees/360), saturation, lightness)
  def pure: Hsv = Hsv(hue, 1, 0)
  
  def srgb: Srgb =
    if saturation == 0 then Srgb(lightness, lightness, lightness)
    else
      val v2 =
        if lightness < 0.5 then lightness*(1 + saturation)
        else (lightness + saturation - saturation*lightness)
      
      val v1 = 2*lightness - v2

      def convert(h: Double): Double =
        val vh = unitary(h)
        if 6*vh < 1 then v1 + (v2 - v1)*6*vh
        else if 2*vh < 1 then v2
        else if 3*vh < 2 then v1 + (v2 - v1)*((2.0/3) - vh)*6
        else v1

      Srgb(convert(hue + (1.0/3.0)), convert(hue), convert(hue - (1.0/3.0)))
  
  def css: Text =
    Text(s"hsl(${(hue*360).toInt}, ${(saturation*100).toInt}%, ${(lightness*100).toInt}%)")
