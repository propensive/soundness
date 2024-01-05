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

import probably.*
import rudiments.*
import gossamer.*

given ColorProfile = colorProfiles.daylight

object Tests extends Suite(t"Iridescence tests"):
  def run(): Unit =
    // suite(t"Roundtrip tests"):
    //   for color <- colors.all.reverse do
    //     test(t"sRGB to L*a*b*"):
    //       color.srgb.cielab.srgb
    //     .assert(_ ~~ color.srgb)

    //     test(t"HSV to sRGB and back"):
    //       color.srgb.hsv.srgb.hsv
    //     .assert(_ ~~ color.srgb.hsv)
        
    //     test(t"sRGB to CMY and back"):
    //       color.srgb.cmy.srgb
    //     .assert(_ ~~ color.srgb)
        
    //     test(t"sRGB to CMYK and back"):
    //       color.srgb.cmyk.srgb
    //     .assert(_ ~~ color.srgb)
        
    //     test(t"sRGB to XYZ and back"):
    //       color.srgb.xyz.srgb
    //     .assert(_ ~~ color.srgb)
        
    //     test(t"sRGB to HSL and back"):
    //       color.srgb.hsl.srgb
    //     .assert(_ ~~ color.srgb)

    suite(t"Interpolator tests"):
      test(t"Read a hex value with a leading hash"):
        rgb"#abcdef"
      .assert(_ == Rgb24(171, 205, 239))
      
      test(t"Read a hex value without a leading hash"):
        rgb"abcdef"
      .assert(_ == Rgb24(171, 205, 239))
      
      test(t"Read black"):
        rgb"#000000"
      .assert(_ == Rgb24(0, 0, 0))
      
      test(t"Read white"):
        rgb"#ffffff"
      .assert(_ == Rgb24(255, 255, 255))
