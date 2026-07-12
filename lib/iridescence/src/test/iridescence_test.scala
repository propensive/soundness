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
┃    Soundness, version 0.63.0.                                                                    ┃
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
          color.to[Cielab].to[Srgb]
        . assert(_ === color.to[Srgb])

        test(m"HSV to sRGB and back"):
          color.to[Hsv].to[Srgb]
        . assert(_ === color.to[Srgb])

        test(m"sRGB to CMY and back"):
          color.to[Cmy].to[Srgb]
        . assert(_ === color.to[Srgb])

        test(m"sRGB to CMYK and back"):
          color.to[Cmyk].to[Srgb]
        . assert(_ === color.to[Srgb])

        test(m"sRGB to XYZ and back"):
          color.to[Xyz].to[Srgb]
        . assert(_ === color.to[Srgb])

        test(m"sRGB to HSL and back"):
          color.to[Hsl].to[Srgb]
        . assert(_ === color.to[Srgb])

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

    suite(m"Hsl manipulation"):
      test(m"saturate stays in Hsl"):
        Hsl(0.5, 0.3, 0.4).saturate
      . assert(_ == Hsl(0.5, 1.0, 0.4))

      test(m"desaturate stays in Hsl"):
        Hsl(0.5, 0.3, 0.4).desaturate
      . assert(_ == Hsl(0.5, 0.0, 0.4))

      test(m"rotate wraps around 360 degrees"):
        Hsl(0.5, 0.3, 0.4).rotate(360).hue
      . assert(_ == 0.5)

      test(m"rotate by 180 produces the complement"):
        Hsl(0.25, 0.3, 0.4).rotate(180).hue
      . assert(_ == 0.75)

      test(m"complement matches a 180-degree rotation"):
        Hsl(0.25, 0.3, 0.4).complement
      . assert(_ == Hsl(0.25, 0.3, 0.4).rotate(180))

      test(m"pure has maximum saturation at mid lightness"):
        Hsl(0.3, 0.2, 0.7).pure
      . assert(_ == Hsl(0.3, 1.0, 0.5))

      test(m"lighten moves halfway toward 1"):
        Hsl(0.5, 0.3, 0.4).lighten(0.5).lightness
      . assert(_ == 0.7)

      test(m"darken moves halfway toward 0"):
        Hsl(0.5, 0.3, 0.4).darken(0.5).lightness
      . assert(_ == 0.2)

    suite(m"Hsv manipulation"):
      test(m"saturate stays in Hsv"):
        Hsv(0.5, 0.3, 0.4).saturate
      . assert(_ == Hsv(0.5, 1.0, 0.4))

      test(m"desaturate stays in Hsv"):
        Hsv(0.5, 0.3, 0.4).desaturate
      . assert(_ == Hsv(0.5, 0.0, 0.4))

      test(m"complement matches a 180-degree rotation"):
        Hsv(0.25, 0.3, 0.4).complement
      . assert(_ == Hsv(0.25, 0.3, 0.4).rotate(180))

      test(m"pure has maximum saturation and value"):
        Hsv(0.3, 0.2, 0.7).pure
      . assert(_ == Hsv(0.3, 1.0, 1.0))

      test(m"shade(0) is a no-op"):
        Hsv(0.5, 0.3, 0.4).shade(0)
      . assert(_ == Hsv(0.5, 0.3, 0.4))

      test(m"shade(1) drives value to 0"):
        Hsv(0.5, 0.3, 0.4).shade(1).value
      . assert(_ == 0.0)

      test(m"tint(0) is a no-op"):
        Hsv(0.5, 0.3, 0.4).tint(0)
      . assert(_ == Hsv(0.5, 0.3, 0.4))

      test(m"tint(1) drives saturation to 0"):
        Hsv(0.5, 0.3, 0.4).tint(1).saturation
      . assert(_ == 0.0)

    suite(m"Srgb manipulation"):
      test(m"mix at 0 returns the receiver"):
        Srgb(0.2, 0.4, 0.6).mix(Srgb(0.8, 0.6, 0.4), 0)
      . assert(_ == Srgb(0.2, 0.4, 0.6))

      test(m"mix at 1 returns the argument"):
        Srgb(0.2, 0.4, 0.6).mix(Srgb(0.8, 0.6, 0.4), 1)
      . assert(_ == Srgb(0.8, 0.6, 0.4))

      test(m"mix at 0.5 averages the channels"):
        Srgb(0.2, 0.4, 0.6).mix(Srgb(0.8, 0.6, 0.4))
      . assert(_ == Srgb(0.5, 0.5, 0.5))

    suite(m"Cielab manipulation"):
      test(m"delta to self is zero"):
        Cielab(50, 10, -20).delta(Cielab(50, 10, -20))
      . assert(_ == 0.0)

      test(m"delta is a 3-D Euclidean distance"):
        Cielab(0, 0, 0).delta(Cielab(3, 4, 12))
      . assert(_ == 13.0)

      test(m"mix at 0.5 averages all axes"):
        Cielab(0, 0, 0).mix(Cielab(40, 20, 10))
      . assert(_ == Cielab(20, 10, 5))
