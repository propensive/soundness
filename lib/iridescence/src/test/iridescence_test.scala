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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package iridescence

import soundness.*

given Colorimetry = colorimetry.daylight

object Tests extends Suite(m"Iridescence tests"):
  def run(): Unit =
    suite(m"Roundtrip tests"):

      given Srgb is Checkable against Srgb = (left, right) =>
        left.red === (right.red +/- 0.01)
        && left.green === (right.green +/- 0.01)
        && left.blue === (right.blue +/- 0.01)

      given Hsv is Checkable against Hsv = (left, right) =>
        left.hue === (right.hue +/- 0.05)
        && left.saturation === (right.saturation +/- 0.05)
        && left.value === (right.value +/- 0.05)

      for color <- WebColors.colors.reverse do
        test(m"sRGB to L*a*b*"):
          color.in[Srgb].in[Cielab].in[Srgb]
        . assert(_ === color.srgb)

        test(m"HSV to sRGB and back"):
          color.in[Srgb].in[Hsv].in[Srgb]
        . assert(_ === color.hsv)

        test(m"sRGB to CMY and back"):
          color.in[Srgb].in[Cmy].in[Srgb]
        . assert(_ === color.srgb)

        test(m"sRGB to CMYK and back"):
          color.in[Srgb].in[Cmyk].in[Srgb]
        . assert(_ === color.srgb)

        test(m"sRGB to XYZ and back"):
          color.in[Srgb].in[Xyz].in[Srgb]
        . assert(_ === color.srgb)

        test(m"sRGB to HSL and back"):
          color.in[Srgb].in[Hsl].in[Srgb]
        . assert(_ === color.srgb)

    suite(m"Interpolator tests"):
      test(m"Read a hex value with a leading hash"):
        rgb"#abcdef"
      . assert(_ == Chroma(171, 205, 239))

      test(m"Read a hex value without a leading hash"):
        rgb"abcdef"
      . assert(_ == Chroma(171, 205, 239))

      test(m"Read black"):
        rgb"#000000"
      . assert(_ == Chroma(0, 0, 0))

      test(m"Read white"):
        rgb"#ffffff"
      . assert(_ == Chroma(255, 255, 255))
