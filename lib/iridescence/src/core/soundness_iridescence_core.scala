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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package soundness

export
  iridescence
  . { Blendable, Brightness, Cielab, Cmy, Cmyk, Cmyk8, Color, color, Colorimetry, dark, Daub, Hsl,
      Hsv, light, Mixing, packed, Palette, Perceptual, Pixel, PixelOpaque, rgb, Rgb, Rgba, Rgb12,
      Rgb12Opaque, Rgb32, rgb32, Rgb32Opaque, Solarized, Spectrum, Srgb, Theme, Tonal, WebColors,
      Xyz }

package colorimetry:
  export
    iridescence.colorimetry
    . { adobeRgbColorimetry, coolFluorescentColorimetry, coolWhiteFluorescentColorimetry, d50SimulatorColorimetry, d65SimulatorColorimetry, daylightColorimetry,
        daylightFluorescentF1Colorimetry, daylightFluorescentF5Colorimetry, daylightFluorescentF7Colorimetry, equalEnergyColorimetry,
        iccProfilePcsColorimetry, incandescentTungstenColorimetry, liteWhiteFluorescentColorimetry, midMorningDaylightColorimetry,
        northSkyDaylightColorimetry, oldDaylightColorimetry, oldDirectSunlightAtNoonColorimetry, philipsTl83Colorimetry, philipsTl84Colorimetry,
        philipsTl85Colorimetry, srgbColorimetry, sylvaniaF40Colorimetry, ultralume30Colorimetry, ultralume40Colorimetry, ultralume50Colorimetry, warmWhiteFluorescentColorimetry,
        whiteFluorescentColorimetry }

package luminosity:
  export iridescence.luminosity.{darkBrightness, lightBrightness}

package mixing:
  export
    iridescence.mixing
    . { colorBurnMixing, colorDodgeMixing, darkenMixing, differenceMixing, exclusionMixing, hardLightMixing, lightenMixing, linearBurnMixing,
        linearDodgeMixing, multiplyMixing, overlayMixing, proportionalMixing, screenMixing, softLightMixing }

package themes:
  export iridescence.themes.solarizedTheme
