/*
    Iridescence, version 0.4.0. Copyright 2021-21 Jon Pretty, Propensive OÜ.

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

import probably.*

object Main extends Suite("Iridescence tests"):
  
  given Profile = profiles.Daylight
  given Tolerance[Double] = (a, b) => math.abs(a - b) < 0.05
  
  def run(using Runner): Unit =
    for color <- colors.all.reverse do
      test("sRGB to L*a*b*") {
        color.debug.cielab.srgb.debug
      }.assert(_ ~~ color)

      test("HSV to sRGB and back") {
        color.debug.hsv.srgb.hsv.debug
      }.assert(_ ~~ color.hsv)
      
      test("sRGB to CMY and back") {
        color.debug.cmy.srgb.debug
      }.assert(_ ~~ color)
      
      test("sRGB to CMYK and back") {
        color.debug.cmyk.srgb.debug
      }.assert(_ ~~ color)
      
      test("sRGB to XYZ and back") {
        color.debug.xyz.srgb.debug
      }.assert(_ ~~ color)
      
      test("sRGB to HSL and back") {
        color.debug.hsl.debug.srgb.debug
      }.assert(_ ~~ color)