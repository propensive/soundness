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
package iridescence

import anticipation.*
import prepositional.*

extension (inline context: StringContext)
  transparent inline def rgb(inline parts: Any*): Chroma =
    ${iridescence.internal.rgbMacro('context, 'parts)}

inline def dark(using luminosity: Brightness): Boolean = luminosity == Brightness.Dark
inline def light(using luminosity: Brightness): Boolean = luminosity != Brightness.Dark

package themes:
  given solarizedTheme: Brightness => Theme = new Theme with Solarized:
    val luminosity = summon[Brightness]
    val background = if dark then base03 else base3
    val foreground = if dark then base3 else base03

package luminosity:
  given darkBrightness: Brightness = Brightness.Dark
  given lightBrightness: Brightness = Brightness.Light

// The blend modes, named as Photoshop and GIMP name them and defined as the W3C compositing
// specification defines them, for `Daub` arithmetic to add through. All but `proportional` read
// coordinates as fractions of full intensity, so they are offered only for the spaces marked
// `Tonal`; asking for `multiply` in CIELAB, where lightness runs to 100, will not compile.
package mixing:
  // The layer replaces the backdrop, so mixing it back in proportion leaves a plain weighted
  // average — what pairwise `mix` does, and the only mode that means anything in a space whose
  // coordinates are not confined to 0..1.
  given proportional: [topic <: Color] => topic is Mixing = (_, layer) => layer

  given multiply: [topic <: Color: Tonal] => topic is Mixing =
    (backdrop, layer) => backdrop*layer

  given screen: [topic <: Color: Tonal] => topic is Mixing =
    (backdrop, layer) => backdrop + layer - backdrop*layer

  given darken: [topic <: Color: Tonal] => topic is Mixing =
    (backdrop, layer) => math.min(backdrop, layer)

  given lighten: [topic <: Color: Tonal] => topic is Mixing =
    (backdrop, layer) => math.max(backdrop, layer)

  given difference: [topic <: Color: Tonal] => topic is Mixing =
    (backdrop, layer) => math.abs(backdrop - layer)

  given exclusion: [topic <: Color: Tonal] => topic is Mixing =
    (backdrop, layer) => backdrop + layer - 2*backdrop*layer

  given linearDodge: [topic <: Color: Tonal] => topic is Mixing =
    (backdrop, layer) => math.min(1.0, backdrop + layer)

  given linearBurn: [topic <: Color: Tonal] => topic is Mixing =
    (backdrop, layer) => math.max(0.0, backdrop + layer - 1)

  given hardLight: [topic <: Color: Tonal] => topic is Mixing =
    (backdrop, layer) =>
      if layer <= 0.5 then 2*backdrop*layer else 1 - 2*(1 - backdrop)*(1 - layer)

  // Overlay is hard light with the two operands exchanged: the backdrop, rather than the layer,
  // decides whether the pair is multiplied or screened.
  given overlay: [topic <: Color: Tonal] => topic is Mixing =
    (backdrop, layer) =>
      if backdrop <= 0.5 then 2*backdrop*layer else 1 - 2*(1 - backdrop)*(1 - layer)

  given softLight: [topic <: Color: Tonal] => topic is Mixing =
    (backdrop, layer) =>
      if layer <= 0.5 then backdrop - (1 - 2*layer)*backdrop*(1 - backdrop) else
        val toward =
          if backdrop <= 0.25 then ((16*backdrop - 12)*backdrop + 4)*backdrop
          else math.sqrt(backdrop)

        backdrop + (2*layer - 1)*(toward - backdrop)

  given colorDodge: [topic <: Color: Tonal] => topic is Mixing =
    (backdrop, layer) =>
      if backdrop == 0.0 then 0.0
      else if layer == 1.0 then 1.0
      else math.min(1.0, backdrop/(1 - layer))

  given colorBurn: [topic <: Color: Tonal] => topic is Mixing =
    (backdrop, layer) =>
      if backdrop == 1.0 then 1.0
      else if layer == 0.0 then 0.0
      else 1 - math.min(1.0, (1 - backdrop)/layer)

package colorimetry:
  given incandescentTungsten: Colorimetry = Colorimetry(109.850, 100, 35.585, 111.144, 100, 35.2)

  given oldDirectSunlightAtNoon: Colorimetry =
    Colorimetry(99.0927, 100, 85.313, 99.178, 100, 84.3493)

  given oldDaylight: Colorimetry = Colorimetry(98.074, 100, 118.232, 97.285, 100, 116.145)
  given iccProfilePcs: Colorimetry = Colorimetry(96.422, 100, 82.521, 96.720, 100, 81.427)
  given midMorningDaylight: Colorimetry = Colorimetry(95.682, 100, 92.149, 95.799, 100, 90.926)
  given daylight: Colorimetry = Colorimetry(95.047, 100, 108.883, 94.811, 100, 107.304)
  given srgb: Colorimetry = daylight
  given adobeRgb: Colorimetry = daylight
  given northSkyDaylight: Colorimetry = Colorimetry(94.972, 100, 122.638, 94.416, 100, 120.641)
  given equalEnergy: Colorimetry = Colorimetry(100, 100, 100, 100, 100, 100)
  given daylightFluorescentF1: Colorimetry = Colorimetry(92.834, 100, 103.665, 94.791, 100, 103.191)
  given coolFluorescent: Colorimetry = Colorimetry(99.187, 100, 67.395, 103.280, 100, 69.026)
  given whiteFluorescent: Colorimetry = Colorimetry(103.754, 100, 49.861, 108.968, 100, 51.965)
  given warmWhiteFluorescent: Colorimetry = Colorimetry(109.147, 100, 38.813, 114.961, 100, 40.963)
  given daylightFluorescentF5: Colorimetry = Colorimetry(90.872, 100, 98.723, 93.369, 100, 98.636)
  given liteWhiteFluorescent: Colorimetry = Colorimetry(97.309, 100, 60.191, 102.148, 100, 62.074)
  given daylightFluorescentF7: Colorimetry = Colorimetry(95.044, 100, 108.755, 95.792, 100, 107.687)
  given d65Simulator: Colorimetry = daylightFluorescentF7
  given sylvaniaF40: Colorimetry = Colorimetry(96.413, 100, 82.333, 97.115, 100, 81.135)
  given d50Simulator: Colorimetry = sylvaniaF40
  given coolWhiteFluorescent: Colorimetry = Colorimetry(100.365, 100, 67.868, 102.116, 100, 67.826)
  given philipsTl85: Colorimetry = Colorimetry(96.174, 100, 81.712, 99.001, 100, 83.134)
  given ultralume50: Colorimetry = philipsTl85
  given philipsTl84: Colorimetry = Colorimetry(100.966, 100, 64.370, 103.866, 100, 65.627)
  given ultralume40: Colorimetry = philipsTl84
  given philipsTl83: Colorimetry = Colorimetry(108.046, 100, 39.228, 111.428, 100, 40.353)
  given ultralume30: Colorimetry = philipsTl83

package palettes:
  trait Reporting:
    palette: Palette =>
      def success: Color
      def error: Color
      def warning: Color

  trait Terminal:
    palette: Palette =>
      def black: Color
      def red: Color
      def green: Color
      def yellow: Color
      def blue: Color
      def magenta: Color
      def cyan: Color
      def white: Color
      def brightBlack: Color
      def brightRed: Color
      def brightGreen: Color
      def brightYellow: Color
      def brightBlue: Color
      def brightMagenta: Color
      def brightCyan: Color
      def brightWhite: Color

  trait Syntax:
    palette: Palette =>
      def error: Color
      def number: Color
      def modifier: Color
      def identifier: Color
      def term: Color
      def meta: Color
      def string: Color
      def parenthesis: Color
      def symbol: Color
      def comment: Color
