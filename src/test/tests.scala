/*
    Iridescence, version 0.8.0. Copyright 2021-21 Jon Pretty, Propensive OÜ.

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
import eucalyptus.*
import rudiments.*
import gossamer.*

import unsafeExceptions.canThrowAny

given Log(Everything |-> Stdout)

object Tests extends Suite(t"Iridescence tests"):
  
  given Profile = profiles.Daylight
  given Tolerance[Double] = (a, b) => math.abs(a - b) < 0.05
  
  def run(using Runner): Unit =
    for color <- colors.all.reverse do
      test(t"sRGB to L*a*b*") {
        color.cielab.srgb
      }.assert(_ ~~ color)

      test(t"HSV to sRGB and back") {
        color.hsv.srgb.hsv
      }.assert(_ ~~ color.hsv)
      
      test(t"sRGB to CMY and back") {
        color.cmy.srgb
      }.assert(_ ~~ color)
      
      test(t"sRGB to CMYK and back") {
        color.cmyk.srgb
      }.assert(_ ~~ color)
      
      test(t"sRGB to XYZ and back") {
        color.xyz.srgb
      }.assert(_ ~~ color)
      
      test(t"sRGB to HSL and back") {
        color.hsl.srgb
      }.assert(_ ~~ color)
