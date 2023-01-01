/*
    Iridescence, version 0.4.0. Copyright 2021-23 Jon Pretty, Propensive OÜ.

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

import rudiments.*

import language.experimental.captureChecking

object colorProfiles:
  val IncandescentTungsten = ColorProfile(109.850, 100, 35.585, 111.144, 100, 35.200)
  val OldDirectSunlightAtNoon = ColorProfile(99.0927, 100, 85.313, 99.178, 100, 84.3493)
  val OldDaylight = ColorProfile(98.074, 100, 118.232, 97.285, 100, 116.145)
  val IccProfilePcs = ColorProfile(96.422, 100, 82.521, 96.720, 100, 81.427)
  val MidMorningDaylight = ColorProfile(95.682, 100, 92.149, 95.799, 100, 90.926)
  val Daylight, Srgb, AdobeRgb = ColorProfile(95.047, 100, 108.883, 94.811, 100, 107.304)
  val NorthSkyDaylight = ColorProfile(94.972, 100, 122.638, 94.416, 100, 120.641)
  val EqualEnergy = ColorProfile(100, 100, 100, 100, 100, 100)
  val DaylightFluorescentF1 = ColorProfile(92.834, 100, 103.665, 94.791, 100, 103.191)
  val CoolFluorescent = ColorProfile(99.187, 100, 67.395, 103.280, 100, 69.026)
  val WhiteFluorescent = ColorProfile(103.754, 100, 49.861, 108.968, 100, 51.965)
  val WarmWhiteFluorescent = ColorProfile(109.147, 100, 38.813, 114.961, 100, 40.963)
  val DaylightFluorescentF5 = ColorProfile(90.872, 100, 98.723, 93.369, 100, 98.636)
  val LiteWhiteFluorescent = ColorProfile(97.309, 100, 60.191, 102.148, 100, 62.074)
  val DaylightFluorescentF7, D65Simulator  = ColorProfile(95.044, 100, 108.755, 95.792, 100, 107.687)
  val SylvaniaF40, D50Simulator  = ColorProfile(96.413, 100, 82.333, 97.115, 100, 81.135)
  val CoolWhiteFluorescent = ColorProfile(100.365, 100, 67.868, 102.116, 100, 67.826)
  val Ultralume50, PhilipsTl85 = ColorProfile(96.174, 100, 81.712, 99.001, 100, 83.134)
  val Ultralume40, PhilipsTl84 = ColorProfile(100.966, 100, 64.370, 103.866, 100, 65.627)
  val Ultralume30, PhilipsTl83 = ColorProfile(108.046, 100, 39.228, 111.428, 100, 40.353)

@implicitNotFound("a contextual ColorProfile is required, for example by importing one from the "+
                      "iridescence.colorProfiles package, such as,\n\n"+
                      "  import iridescence.colorProfiles.Daylight\n")
case class ColorProfile(x2: Double, y2: Double, z2: Double, x10: Double, y10: Double, z10: Double)