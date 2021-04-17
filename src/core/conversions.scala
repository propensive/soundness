/*

    Iridesce, version 0.2.0. Copyright 2021-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package iridescence

trait Color

object Color:
  private[iridescence] def unitary(d: Double): Double = d - d.toInt + (if d < 0 then 1 else 0)

case class Xyz(x: Double, y: Double, z: Double) extends Color:
  
  def luminescence: Double = y
  
  def srgb: Srgb =
    def limit(v: Double): Double = if v > 0.0031308 then 1.055*math.pow(v, 1/2.4) - 0.055 else 12.92*v
    
    val red = limit(x*0.032406994 - y*0.0153738318 - z*0.0049861076)
    val green = limit(-x*0.0096924364 + y*0.01878675 + z*0.0004155506)
    val blue = limit(x*0.0005563008 - y*0.0020397696 + z*0.0105697151)

    Srgb(red, green, blue)
  
  def cielab(using profile: Profile): Cielab =
    def limit(v: Double): Double = if v > 0.008856 then math.pow(v, 1.0/3) else 7.787*v + 0.13793

    val l = 116*limit(y/profile.y2) - 16
    val a = 500*(limit(x/profile.x2) - limit(y/profile.y2))
    val b = 200*(limit(y/profile.y2) - limit(z/profile.z2))

    Cielab(l, a, b)

case class Srgb(red: Double, green: Double, blue: Double) extends Color:
  def css: String = s"rgb(${(red*255).toInt}, ${(green*255).toInt}, ${(blue*255).toInt})"
  def ansiFg24: String = s"${27.toChar}[38;2;${(red*255).toInt};${(green*255).toInt};${(blue*255).toInt}m"
  def ansiBg24: String = s"${27.toChar}[48;2;${(red*255).toInt};${(green*255).toInt};${(blue*255).toInt}m"
  def hex12: String = Seq(red, green, blue).map { c => Integer.toHexString((c*16).toInt) }.mkString("#", "", "")
  
  def hex24: String =
    Seq(red, green, blue).map { c =>
      val hex = Integer.toHexString((c*255).toInt)
      if hex.length < 2 then s"0$hex" else hex
    }.mkString("#", "", "")

  def xyz(using profile: Profile): Xyz =
    def limit(v: Double): Double = if v > 0.04045 then math.pow((v + 0.055)/1.055, 2.4) else v/12.92

    val List(r, g, b) = List(red, green, blue).map(limit(_)*100)

    val x = r*0.4124 + g*0.3576 + b*0.1805
    val y = r*0.2126 + g*0.7152 + b*0.0722
    val z = r*0.0193 + g*0.1192 + b*0.9505

    Xyz(x, y, z)
  
  def cielab(using profile: Profile): Cielab = xyz.cielab
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

      Hsl(Color.unitary(hue), saturation, lightness)

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
      val hue = if value == red then db - dg else if value == green then 1.0/3 + dr - db else 2.0/3 + dg - dr

      Hsv(Color.unitary(hue), saturation, value)

case class Cielab(l: Double, a: Double, b: Double) extends Color:
  def srgb(using Profile): Srgb = xyz.srgb

  def xyz(using profile: Profile): Xyz =
    def limit(v: Double): Double = if v*v*v > 0.008856 then v*v*v else (v - 16.0/116)/7.787
  
    val y = limit((l + 16)/116)*profile.y2
    val x = limit(a/500 + (l + 16)/116)*profile.x2
    val z = limit((l + 16)/116 - b/200)*profile.z2

    Xyz(x, y, z)

  def mix(that: Cielab, ratio: Double = 0.5): Cielab =
    Cielab(l*(1 - ratio) + ratio*that.l, a*(1 - ratio) + ratio*that.a, b*(1 - ratio) + ratio*that.b)

  def delta(that: Cielab): Double = math.sqrt(that.a*that.a + that.b*that.b) - math.sqrt(a*a + b*b)

case class Cmy(cyan: Double, magenta: Double, yellow: Double) extends Color:
  def srgb: Srgb = Srgb((1 - cyan), (1 - magenta), (1 - yellow))
  def cmyk: Cmyk =
    val key = List(1, cyan, magenta, yellow).min
    
    if key == 1 then Cmyk(0, 0, 0, 1)
    else Cmyk((cyan - key)/(1 - key), (magenta - key)/(1 - key), (yellow - key)/(1 - key), key)

case class Cmyk(cyan: Double, magenta: Double, yellow: Double, key: Double) extends Color:
  def srgb: Srgb = cmy.srgb
  def cmy: Cmy = Cmy(cyan*(1 - key) + key, magenta*(1 - key) + key, yellow*(1 - key) + key)

case class Hsv(hue: Double, saturation: Double, value: Double) extends Color:
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
  
  def saturate: Hsv = Hsv(hue, 1, value)
  def desaturate: Hsv = Hsv(hue, 0, value)
  def rotate(degrees: Double): Hsv = Hsv(Color.unitary(hue + degrees/360), saturation, value)
  def pure: Hsv = Hsv(hue, 1, 0)
  
  def shade(black: Double = 0): Hsv = Hsv(hue, saturation, value*(1 - black) + (1 - value)*black)
  def tint(white: Double = 0): Hsv = Hsv(hue, saturation*(1 - white) + (1 - saturation)*white, value)
  def tone(black: Double = 0, white: Double = 0) = shade(black).tint(white)

case class Hsl(hue: Double, saturation: Double, lightness: Double) extends Color:
  def srgb: Srgb =
    if saturation == 0 then Srgb(lightness, lightness, lightness)
    else
      val v2 =
        if lightness < 0.5 then lightness*(1 + saturation)
        else (lightness + saturation - saturation*lightness)
      
      val v1 = 2*lightness - v2

      def convert(h: Double): Double =
        val vh = Color.unitary(h)
        if 6*vh < 1 then v1 + (v2 - v1)*6*vh
        else if 2*vh < 1 then v2
        else if 3*vh < 2 then v1 + (v2 - v1)*((2.0/3) - vh)*6
        else v1

      Srgb(convert(hue + (1.0/3.0)), convert(hue), convert(hue - (1.0/3.0)))
  
  def css: String = s"hsl(${(hue*360).toInt}, ${(saturation*100).toInt}%, ${(lightness*100).toInt}%)"
  def saturate: Hsv = Hsv(hue, 1, lightness)
  def desaturate: Hsv = Hsv(hue, 0, lightness)
  def rotate(degrees: Double): Hsv = Hsv(Color.unitary(hue + degrees/360), saturation, lightness)
  def pure: Hsv = Hsv(hue, 1, 0)
  