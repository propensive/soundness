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

import language.experimental.captureChecking

object colorProfiles:
  given incandescentTungsten: ColorProfile = ColorProfile(109.850, 100, 35.585, 111.144, 100, 35.2)
  given oldDirectSunlightAtNoon: ColorProfile = ColorProfile(99.0927, 100, 85.313, 99.178, 100, 84.3493)
  given oldDaylight: ColorProfile = ColorProfile(98.074, 100, 118.232, 97.285, 100, 116.145)
  given iccProfilePcs: ColorProfile = ColorProfile(96.422, 100, 82.521, 96.720, 100, 81.427)
  given midMorningDaylight: ColorProfile = ColorProfile(95.682, 100, 92.149, 95.799, 100, 90.926)
  given daylight: ColorProfile = ColorProfile(95.047, 100, 108.883, 94.811, 100, 107.304)
  given srgb: ColorProfile = daylight
  given adobeRgb: ColorProfile = daylight
  given northSkyDaylight: ColorProfile = ColorProfile(94.972, 100, 122.638, 94.416, 100, 120.641)
  given equalEnergy: ColorProfile = ColorProfile(100, 100, 100, 100, 100, 100)
  given daylightFluorescentF1: ColorProfile = ColorProfile(92.834, 100, 103.665, 94.791, 100, 103.191)
  given coolFluorescent: ColorProfile = ColorProfile(99.187, 100, 67.395, 103.280, 100, 69.026)
  given whiteFluorescent: ColorProfile = ColorProfile(103.754, 100, 49.861, 108.968, 100, 51.965)
  given warmWhiteFluorescent: ColorProfile = ColorProfile(109.147, 100, 38.813, 114.961, 100, 40.963)
  given daylightFluorescentF5: ColorProfile = ColorProfile(90.872, 100, 98.723, 93.369, 100, 98.636)
  given liteWhiteFluorescent: ColorProfile = ColorProfile(97.309, 100, 60.191, 102.148, 100, 62.074)
  given daylightFluorescentF7: ColorProfile = ColorProfile(95.044, 100, 108.755, 95.792, 100, 107.687)
  given d65Simulator: ColorProfile = daylightFluorescentF7
  given sylvaniaF40: ColorProfile = ColorProfile(96.413, 100, 82.333, 97.115, 100, 81.135)
  given d50Simulator: ColorProfile = sylvaniaF40
  given coolWhiteFluorescent: ColorProfile = ColorProfile(100.365, 100, 67.868, 102.116, 100, 67.826)
  given philipsTl85: ColorProfile = ColorProfile(96.174, 100, 81.712, 99.001, 100, 83.134)
  given ultralume50: ColorProfile = philipsTl85
  given philipsTl84: ColorProfile = ColorProfile(100.966, 100, 64.370, 103.866, 100, 65.627)
  given ultralume40: ColorProfile = philipsTl84
  given philipsTl83: ColorProfile = ColorProfile(108.046, 100, 39.228, 111.428, 100, 40.353)
  given ultralume30: ColorProfile = philipsTl83

case class ColorProfile(x2: Double, y2: Double, z2: Double, x10: Double, y10: Double, z10: Double)
