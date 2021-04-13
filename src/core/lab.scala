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
package iridesce

trait Color:
  def srgb(using Profile): Srgb
  
  def css(using Profile): String =
    val Srgb(r, g, b) = srgb
    s"rgb(${r.toInt}, ${g.toInt}, ${b.toInt})"
  
  def ansi24(using Profile): String =
    val Srgb(r, g, b) = srgb
    s"${27.toChar}[38;2;${r.toInt};${g.toInt};${b.toInt}m"
  
  def hex(using Profile): String =
    val Srgb(r, g, b) = srgb
    List(r, g, b).map { c =>
      val hex = Integer.toHexString(c.toInt)
      if hex.length < 2 then s"0$hex" else hex
    }.mkString("#", "", "")

object Color:
  def unitary(d: Double): Double = d - d.toInt + (if d < 0.0 then 1 else 0)

case class Xyz(x: Double, y: Double, z: Double) extends Color:
  def srgb(using Profile): Srgb =
    val r = x*0.032406 - y*0.015372 - z*0.004986
    val g = -x*0.009689 + y*0.018758 + z*0.000415
    val b = x*0.000557 - y*0.002040 + z*0.010570
  
    def limit(v: Double): Double = 255*(if v > 0.0031308 then 1.055*math.pow(v, 1/2.4) - 0.055 else 12.92*v)

    Srgb(limit(r), limit(g), limit(b))
  
  def cielab(using profile: Profile): CieLab =
    def limit(v: Double): Double = if v > 0.008856 then math.pow(v, 1.0/3.0) else 7.787*v + 0.13793

    val l = 116*limit(y/profile.y2) - 16
    val a = 500*(limit(x/profile.x2) - limit(y/profile.y2))
    val b = 200*(limit(y/profile.y2) - limit(z/profile.z2))

    CieLab(l, a, b)

case class Srgb(red: Double, green: Double, blue: Double) extends Color:
  def srgb(using Profile): Srgb = this

  def xyz(using profile: Profile): Xyz =
    def limit(v: Double): Double = if v > 10.31475 then math.pow((v/255 + 0.055)/1.055, 2.4) else v/3294.6

    val List(r, g, b) = List(red, green, blue).map(limit(_)*100)

    val x = r*0.4124 + g*0.3576 + b*0.1805
    val y = r*0.2126 + g*0.7152 + b*0.0722
    val z = r*0.0193 + g*0.1192 + b*0.9505

    Xyz(x, y, z)
  
  def cielab(using profile: Profile): CieLab = xyz.cielab
  def cmy: Cmy = Cmy(1 - red/255, 1 - green/255, 1 - blue/255)
  def cmyk: Cmyk = cmy.cmyk

  def hsl: Hsl =
    val r = red/255
    val g = green/255
    val b = blue/255

    val min = r min g min b
    val max = r max g max b
    val delta = max - min

    if delta == 0.0 then Hsl(0.0, 0.0, max)
    else
      val s = delta/max
      val dr = ((max - r)/6) + (delta/2)/delta
      val dg = ((max - g)/6) + (delta/2)/delta
      val db = ((max - b)/6) + (delta/2)/delta

      val hue = if max == r then db - dg else if max == g then 1.0/3.0 + dr - db else 2.0/3.0 + dg - dr

      Hsl(Color.unitary(hue), s, max)

  def hsv: Hsv =
    val r = red/255
    val g = green/255
    val b = blue/255

    val min = r min g min b
    val value = r max g max b
    val delta = value - min

    if delta == 0.0 then Hsv(0.0, 0.0, value)
    else
      val saturation = delta/value
      val dr = ((value - r)/6) + (delta/2)/delta
      val dg = ((value - g)/6) + (delta/2)/delta
      val db = ((value - b)/6) + (delta/2)/delta
      val hue = if value == r then db - dg else if value == g then 1.0/3.0 + dr - db else 2.0/3.0 + dg - dr

      Hsv(Color.unitary(hue), saturation, value)

case class CieLab(l: Double, a: Double, b: Double) extends Color:
  def srgb(using profile: Profile): Srgb = xyz.srgb

  def xyz(using profile: Profile): Xyz =
    def limit(v: Double): Double = if v*v*v > 0.008856 then v*v*v else (v - 16.0/116.0)/7.787
  
    val y = limit((l + 16)/116)*profile.y2
    val x = limit(a/500 + (l + 16)/116)*profile.x2
    val z = limit((l + 16)/116 - b/200)*profile.z2

    Xyz(x, y, z)

  def mix(that: CieLab, ratio: Double = 0.5): CieLab =
    CieLab(l*(1 - ratio) + ratio*that.l, a*(1 - ratio) + ratio*that.a, b*(1 - ratio) + ratio*that.b)

  def delta(that: CieLab): Double = math.sqrt(that.a*that.a + that.b*that.b) - math.sqrt(a*a + b*b)

case class Cmy(cyan: Double, magenta: Double, yellow: Double) extends Color:
  def srgb(using profile: Profile): Srgb = Srgb((1 - cyan)*255, (1 - magenta)*255, (1 - yellow)*255)
  def cmyk: Cmyk =
    val key = List(1, cyan, magenta, yellow).min
    
    if key == 1 then Cmyk(0, 0, 0, 1)
    else Cmyk((cyan - key)/(1 - key), (magenta - key)/(1 - key), (yellow - key)/(1 - key), key)

case class Cmyk(cyan: Double, magenta: Double, yellow: Double, key: Double) extends Color:
  def srgb(using Profile): Srgb = cmy.srgb
  def cmy: Cmy = Cmy(cyan*(1 - key) + key, magenta*(1 - key) + key, yellow*(1 - key) + key)

case class Hsv(hue: Double, saturation: Double, value: Double) extends Color:
  def srgb(using Profile): Srgb =
    if saturation == 0.0 then Srgb(value*255, value*255, value*255)
    else
      val h = hue*6
      val v = value*255
      val i = h.toInt%6
      val a1 = v*(1 - saturation)
      val a2 = v*(1 - saturation*(h - i))
      val a3 = v*(1 - saturation*(1 - (h - i)))

      i match
        case 0 => Srgb(v, a3, a1)
        case 1 => Srgb(a2, v, a1)
        case 2 => Srgb(a1, v, a3)
        case 3 => Srgb(a1, a2, v)
        case 4 => Srgb(a3, a1, v)
        case _ => Srgb(v, a1, a2)
  
  def saturate: Hsv = Hsv(hue, 1.0, value)
  def desaturate: Hsv = Hsv(hue, 0.0, value)
  def rotate(degrees: Double): Hsv = Hsv(Color.unitary(hue + degrees/360), saturation, value)
  def pure: Hsv = Hsv(hue, 1.0, 0.0)
  
  def shade(black: Double = 0.5): Hsv = Hsv(hue, saturation, value*(1 - black) + (1.0 - value)*black)
  def tint(white: Double = 0.5): Hsv = Hsv(hue, saturation*(1 - white) + (1.0 - saturation)*white, value)
  def tone(black: Double = 0.5, white: Double = 0.5) = shade(black).tint(white)

case class Hsl(hue: Double, saturation: Double, lightness: Double) extends Color:
  def srgb(using Profile): Srgb =
    if saturation == 0.0 then Srgb(value*255, value*255, value*255)
    else
      val h = hue*6
      val v = value*255
      val i = h.toInt%6
      val a1 = v*(1 - saturation)
      val a2 = v*(1 - saturation*(h - i))
      val a3 = v*(1 - saturation*(1 - (h - i)))

      i match
        case 0 => Srgb(v, a3, a1)
        case 1 => Srgb(a2, v, a1)
        case 2 => Srgb(a1, v, a3)
        case 3 => Srgb(a1, a2, v)
        case 4 => Srgb(a3, a1, v)
        case _ => Srgb(v, a1, a2)
  
  def saturate: Hsv = Hsv(hue, 1.0, value)
  def desaturate: Hsv = Hsv(hue, 0.0, value)
  def rotate(degrees: Double): Hsv = Hsv(Color.unitary(hue + degrees/360), saturation, value)
  def pure: Hsv = Hsv(hue, 1.0, 0.0)
  
  def shade(black: Double = 0.5): Hsv = Hsv(hue, saturation, value*(1 - black) + (1.0 - value)*black)
  def tint(white: Double = 0.5): Hsv = Hsv(hue, saturation*(1 - white) + (1.0 - saturation)*white, value)
  def tone(black: Double = 0.5, white: Double = 0.5) = shade(black).tint(white)

      

object Profile:
  val IncandescentTungsten = Profile(109.850, 100.0, 35.585, 111.144, 100.0, 35.200)
  val OldDirectSunlightAtNoon = Profile(99.0927, 100.0, 85.313, 99.178, 100.0, 84.3493)
  val OldDaylight = Profile(98.074, 100.0, 118.232, 97.285, 100.0, 116.145)
  val IccProfilePcs = Profile(96.422, 100.0, 82.521, 96.720, 100.0, 81.427)
  val MidMorningDaylight = Profile(95.682, 100.0, 92.149, 95.799, 100.0, 90.926)
  val Daylight, Srgb, AdobeRgb = Profile(95.047, 100.0, 108.883, 94.811, 100.0, 107.304)
  val NorthSkyDaylight = Profile(94.972, 100.0, 122.638, 94.416, 100.0, 120.641)
  val EqualEnergy = Profile(100.0, 100.0, 100.0, 100.0, 100.0, 100.0)
  val DaylightFluorescentF1 = Profile(92.834, 100.0, 103.665, 94.791, 100.0, 103.191)
  val CoolFluorescent = Profile(99.187, 100.0, 67.395, 103.280, 100.0, 69.026)
  val WhiteFluorescent = Profile(103.754, 100.0, 49.861, 108.968, 100.0, 51.965)
  val WarmWhiteFluorescent = Profile(109.147, 100.0, 38.813, 114.961, 100.0, 40.963)
  val DaylightFluorescentF5 = Profile(90.872, 100.0, 98.723, 93.369, 100.0, 98.636)
  val LiteWhiteFluorescent = Profile(97.309, 100.0, 60.191, 102.148, 100.0, 62.074)
  val DaylightFluorescentF7, D65Simulator  = Profile(95.044, 100.0, 108.755, 95.792, 100.0, 107.687)
  val SylvaniaF40, D50Simulator  = Profile(96.413, 100.0, 82.333, 97.115, 100.0, 81.135)
  val CoolWhiteFluorescent = Profile(100.365, 100.0, 67.868, 102.116, 100.0, 67.826)
  val Ultralume50, PhilipsTl85 = Profile(96.174, 100.0, 81.712, 99.001, 100.0, 83.134)
  val Ultralume40, PhilipsTl84 = Profile(100.966, 100.0, 64.370, 103.866, 100.0, 65.627)
  val Ultralume30, PhilipsTl83 = Profile(108.046, 100.0, 39.228, 111.428, 100.0, 40.353)

case class Profile(x2: Double, y2: Double, z2: Double, x10: Double, y10: Double, z10: Double)
