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

import prepositional.*

object WebColors:
  val AliceBlue: Color in Srgb = Srgb(0.941, 0.973, 1)
  val AntiqueWhite: Color in Srgb = Srgb(0.98, 0.922, 0.843)
  val Aquamarine: Color in Srgb = Srgb(0.498, 1, 0.831)
  val Aqua: Color in Srgb = Srgb(0, 1, 1)
  val Azure: Color in Srgb = Srgb(0.941, 1, 1)
  val Beige: Color in Srgb = Srgb(0.961, 0.961, 0.863)
  val Bisque: Color in Srgb = Srgb(1, 0.894, 0.769)
  val Black: Color in Srgb = Srgb(0, 0, 0)
  val BlanchedAlmond: Color in Srgb = Srgb(1, 0.922, 0.804)
  val Blue: Color in Srgb = Srgb(0, 0, 1)
  val BlueViolet: Color in Srgb = Srgb(0.541, 0.169, 0.886)
  val Brown: Color in Srgb = Srgb(0.647, 0.165, 0.165)
  val BurlyWood: Color in Srgb = Srgb(0.871, 0.722, 0.529)
  val CadetBlue: Color in Srgb = Srgb(0.373, 0.62, 0.627)
  val Chartreuse: Color in Srgb = Srgb(0.498, 1, 0)
  val Chocolate: Color in Srgb = Srgb(0.824, 0.412, 0.118)
  val Coral: Color in Srgb = Srgb(1, 0.498, 0.314)
  val CornflowerBlue: Color in Srgb = Srgb(0.392, 0.584, 0.929)
  val Cornsilk: Color in Srgb = Srgb(1, 0.973, 0.863)
  val Crimson: Color in Srgb = Srgb(0.863, 0.078, 0.235)
  val Cyan: Color in Srgb = Srgb(0, 1, 1)
  val DarkBlue: Color in Srgb = Srgb(0, 0, 0.545)
  val DarkCyan: Color in Srgb = Srgb(0, 0.545, 0.545)
  val DarkGoldenrod: Color in Srgb = Srgb(0.722, 0.525, 0.043)
  val DarkGray: Color in Srgb = Srgb(0.663, 0.663, 0.663)
  val DarkGreen: Color in Srgb = Srgb(0, 0.392, 0)
  val DarkKhaki: Color in Srgb = Srgb(0.741, 0.718, 0.42)
  val DarkMagenta: Color in Srgb = Srgb(0.545, 0, 0.545)
  val DarkOliveGreen: Color in Srgb = Srgb(0.333, 0.42, 0.184)
  val DarkOrange: Color in Srgb = Srgb(1, 0.549, 0)
  val DarkOrchid: Color in Srgb = Srgb(0.6, 0.196, 0.8)
  val DarkRed: Color in Srgb = Srgb(0.545, 0, 0)
  val DarkSalmon: Color in Srgb = Srgb(0.914, 0.588, 0.478)
  val DarkSeaGreen: Color in Srgb = Srgb(0.561, 0.737, 0.545)
  val DarkSlateBlue: Color in Srgb = Srgb(0.282, 0.239, 0.545)
  val DarkSlateGray: Color in Srgb = Srgb(0.184, 0.31, 0.31)
  val DarkTurquoise: Color in Srgb = Srgb(0, 0.808, 0.82)
  val DarkViolet: Color in Srgb = Srgb(0.58, 0, 0.827)
  val DeepPink: Color in Srgb = Srgb(1, 0.078, 0.576)
  val DeepSkyBlue: Color in Srgb = Srgb(0, 0.749, 1)
  val DimGray: Color in Srgb = Srgb(0.412, 0.412, 0.412)
  val DodgerBlue: Color in Srgb = Srgb(0.118, 0.565, 1)
  val FireBrick: Color in Srgb = Srgb(0.698, 0.133, 0.133)
  val FloralWhite: Color in Srgb = Srgb(1, 0.98, 0.941)
  val ForestGreen: Color in Srgb = Srgb(0.133, 0.545, 0.133)
  val Fuchsia: Color in Srgb = Srgb(1, 0, 1)
  val Gainsboro: Color in Srgb = Srgb(0.863, 0.863, 0.863)
  val GhostWhite: Color in Srgb = Srgb(0.973, 0.973, 1)
  val Goldenrod: Color in Srgb = Srgb(0.855, 0.647, 0.125)
  val Gold: Color in Srgb = Srgb(1, 0.843, 0)
  val Gray: Color in Srgb = Srgb(0.502, 0.502, 0.502)
  val Green: Color in Srgb = Srgb(0, 0.502, 0)
  val GreenYellow: Color in Srgb = Srgb(0.678, 1, 0.184)
  val HoneyDew: Color in Srgb = Srgb(0.941, 1, 0.941)
  val HotPink: Color in Srgb = Srgb(1, 0.412, 0.706)
  val IndianRed: Color in Srgb = Srgb(0.804, 0.361, 0.361)
  val Indigo: Color in Srgb = Srgb(0.294, 0, 0.51)
  val Ivory: Color in Srgb = Srgb(1, 1, 0.941)
  val Khaki: Color in Srgb = Srgb(0.941, 0.902, 0.549)
  val LavenderBlush: Color in Srgb = Srgb(1, 0.941, 0.961)
  val Lavender: Color in Srgb = Srgb(0.902, 0.902, 0.98)
  val LawnGreen: Color in Srgb = Srgb(0.486, 0.988, 0)
  val LemonChiffon: Color in Srgb = Srgb(1, 0.98, 0.804)
  val LightBlue: Color in Srgb = Srgb(0.678, 0.847, 0.902)
  val LightCoral: Color in Srgb = Srgb(0.941, 0.502, 0.502)
  val LightCyan: Color in Srgb = Srgb(0.878, 1, 1)
  val LightGoldenrodYellow: Color in Srgb = Srgb(0.98, 0.98, 0.824)
  val LightGray: Color in Srgb = Srgb(0.827, 0.827, 0.827)
  val LightGreen: Color in Srgb = Srgb(0.565, 0.933, 0.565)
  val LightPink: Color in Srgb = Srgb(1, 0.714, 0.757)
  val LightSalmon: Color in Srgb = Srgb(1, 0.627, 0.478)
  val LightSeaGreen: Color in Srgb = Srgb(0.125, 0.698, 0.667)
  val LightSkyBlue: Color in Srgb = Srgb(0.529, 0.808, 0.98)
  val LightSlateGray: Color in Srgb = Srgb(0.467, 0.533, 0.6)
  val LightSteelBlue: Color in Srgb = Srgb(0.69, 0.769, 0.871)
  val LightYellow: Color in Srgb = Srgb(1, 1, 0.878)
  val LimeGreen: Color in Srgb = Srgb(0.196, 0.804, 0.196)
  val Lime: Color in Srgb = Srgb(0, 1, 0)
  val Linen: Color in Srgb = Srgb(0.98, 0.941, 0.902)
  val Magenta: Color in Srgb = Srgb(1, 0, 1)
  val Maroon: Color in Srgb = Srgb(0.502, 0, 0)
  val MediumAquamarine: Color in Srgb = Srgb(0.4, 0.804, 0.667)
  val MediumBlue: Color in Srgb = Srgb(0, 0, 0.804)
  val MediumOrchid: Color in Srgb = Srgb(0.729, 0.333, 0.827)
  val MediumPurple: Color in Srgb = Srgb(0.576, 0.439, 0.859)
  val MediumSeaGreen: Color in Srgb = Srgb(0.235, 0.702, 0.443)
  val MediumSlateBlue: Color in Srgb = Srgb(0.482, 0.408, 0.933)
  val MediumSpringGreen: Color in Srgb = Srgb(0, 0.98, 0.604)
  val MediumTurquoise: Color in Srgb = Srgb(0.282, 0.82, 0.8)
  val MediumVioletRed: Color in Srgb = Srgb(0.78, 0.082, 0.522)
  val MidnightBlue: Color in Srgb = Srgb(0.098, 0.098, 0.439)
  val MintCream: Color in Srgb = Srgb(0.961, 1, 0.98)
  val MistyRose: Color in Srgb = Srgb(1, 0.894, 0.882)
  val Moccasin: Color in Srgb = Srgb(1, 0.894, 0.71)
  val NavajoWhite: Color in Srgb = Srgb(1, 0.871, 0.678)
  val Navy: Color in Srgb = Srgb(0, 0, 0.502)
  val OldLace: Color in Srgb = Srgb(0.992, 0.961, 0.902)
  val OliveDrab: Color in Srgb = Srgb(0.42, 0.557, 0.137)
  val Olive: Color in Srgb = Srgb(0.502, 0.502, 0)
  val OrangeRed: Color in Srgb = Srgb(1, 0.271, 0)
  val Orange: Color in Srgb = Srgb(1, 0.647, 0)
  val Orchid: Color in Srgb = Srgb(0.855, 0.439, 0.839)
  val PaleGoldenrod: Color in Srgb = Srgb(0.933, 0.91, 0.667)
  val PaleGreen: Color in Srgb = Srgb(0.596, 0.984, 0.596)
  val PaleTurquoise: Color in Srgb = Srgb(0.686, 0.933, 0.933)
  val PaleVioletRed: Color in Srgb = Srgb(0.859, 0.439, 0.576)
  val PapayaWhip: Color in Srgb = Srgb(1, 0.937, 0.835)
  val PeachPuff: Color in Srgb = Srgb(1, 0.855, 0.725)
  val Peru: Color in Srgb = Srgb(0.804, 0.522, 0.247)
  val Pink: Color in Srgb = Srgb(1, 0.753, 0.796)
  val Plum: Color in Srgb = Srgb(0.867, 0.627, 0.867)
  val PowderBlue: Color in Srgb = Srgb(0.69, 0.878, 0.902)
  val Purple: Color in Srgb = Srgb(0.502, 0, 0.502)
  val RebeccaPurple: Color in Srgb = Srgb(0.4, 0.2, 0.6)
  val Red: Color in Srgb = Srgb(1, 0, 0)
  val RosyBrown: Color in Srgb = Srgb(0.737, 0.561, 0.561)
  val RoyalBlue: Color in Srgb = Srgb(0.255, 0.412, 0.882)
  val SaddleBrown: Color in Srgb = Srgb(0.545, 0.271, 0.075)
  val Salmon: Color in Srgb = Srgb(0.98, 0.502, 0.447)
  val SandyBrown: Color in Srgb = Srgb(0.957, 0.643, 0.376)
  val SeaGreen: Color in Srgb = Srgb(0.18, 0.545, 0.341)
  val SeaShell: Color in Srgb = Srgb(1, 0.961, 0.933)
  val Sienna: Color in Srgb = Srgb(0.627, 0.322, 0.176)
  val Silver: Color in Srgb = Srgb(0.753, 0.753, 0.753)
  val SkyBlue: Color in Srgb = Srgb(0.529, 0.808, 0.922)
  val SlateBlue: Color in Srgb = Srgb(0.416, 0.353, 0.804)
  val SlateGray: Color in Srgb = Srgb(0.439, 0.502, 0.565)
  val Snow: Color in Srgb = Srgb(1, 0.98, 0.98)
  val SpringGreen: Color in Srgb = Srgb(0, 1, 0.498)
  val SteelBlue: Color in Srgb = Srgb(0.275, 0.51, 0.706)
  val Tan: Color in Srgb = Srgb(0.824, 0.706, 0.549)
  val Teal: Color in Srgb = Srgb(0, 0.502, 0.502)
  val Thistle: Color in Srgb = Srgb(0.847, 0.749, 0.847)
  val Tomato: Color in Srgb = Srgb(1, 0.388, 0.278)
  val Turquoise: Color in Srgb = Srgb(0.251, 0.878, 0.816)
  val Violet: Color in Srgb = Srgb(0.933, 0.51, 0.933)
  val Wheat: Color in Srgb = Srgb(0.961, 0.871, 0.702)
  val WhiteSmoke: Color in Srgb = Srgb(0.961, 0.961, 0.961)
  val White: Color in Srgb = Srgb(1, 1, 1)
  val YellowGreen: Color in Srgb = Srgb(0.604, 0.804, 0.196)
  val Yellow: Color in Srgb = Srgb(1, 1, 0)

  val colors: List[Color in Srgb] =
    List
      ( AliceBlue, AntiqueWhite, Aquamarine, Aqua, Azure, Beige, Bisque, Black, BlanchedAlmond,
        Blue, BlueViolet, Brown, BurlyWood, CadetBlue, Chartreuse, Chocolate, Coral, CornflowerBlue,
        Cornsilk, Crimson, Cyan, DarkBlue, DarkCyan, DarkGoldenrod, DarkGray, DarkGreen, DarkKhaki,
        DarkMagenta, DarkOliveGreen, DarkOrange, DarkOrchid, DarkRed, DarkSalmon, DarkSeaGreen,
        DarkSlateBlue, DarkSlateGray, DarkTurquoise, DarkViolet, DeepPink, DeepSkyBlue, DimGray,
        DodgerBlue, FireBrick, FloralWhite, ForestGreen, Fuchsia, Gainsboro, GhostWhite, Goldenrod,
        Gold, Gray, Green, GreenYellow, HoneyDew, HotPink, IndianRed, Indigo, Ivory, Khaki,
        LavenderBlush, Lavender, LawnGreen, LemonChiffon, LightBlue, LightCoral, LightCyan,
        LightGoldenrodYellow, LightGray, LightGreen, LightPink, LightSalmon, LightSeaGreen,
        LightSkyBlue, LightSlateGray, LightSteelBlue, LightYellow, LimeGreen, Lime, Linen, Magenta,
        Maroon, MediumAquamarine, MediumBlue, MediumOrchid, MediumPurple, MediumSeaGreen,
        MediumSlateBlue, MediumSpringGreen, MediumTurquoise, MediumVioletRed, MidnightBlue,
        MintCream, MistyRose, Moccasin, NavajoWhite, Navy, OldLace, OliveDrab, Olive, OrangeRed,
        Orange, Orchid, PaleGoldenrod, PaleGreen, PaleTurquoise, PaleVioletRed, PapayaWhip,
        PeachPuff, Peru, Pink, Plum, PowderBlue, Purple, RebeccaPurple, Red, RosyBrown, RoyalBlue,
        SaddleBrown, Salmon, SandyBrown, SeaGreen, SeaShell, Sienna, Silver, SkyBlue, SlateBlue,
        SlateGray, Snow, SpringGreen, SteelBlue, Tan, Teal, Thistle, Tomato, Turquoise, Violet,
        Wheat, WhiteSmoke, White, YellowGreen, Yellow )
