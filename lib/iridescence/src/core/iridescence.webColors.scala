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

object webColors:
  val AliceBlue = Srgb(0.941, 0.973, 1).chroma
  val AntiqueWhite = Srgb(0.98, 0.922, 0.843).chroma
  val Aquamarine = Srgb(0.498, 1, 0.831).chroma
  val Aqua = Srgb(0, 1, 1).chroma
  val Azure = Srgb(0.941, 1, 1).chroma
  val Beige = Srgb(0.961, 0.961, 0.863).chroma
  val Bisque = Srgb(1, 0.894, 0.769).chroma
  val Black = Srgb(0, 0, 0).chroma
  val BlanchedAlmond = Srgb(1, 0.922, 0.804).chroma
  val Blue = Srgb(0, 0, 1).chroma
  val BlueViolet = Srgb(0.541, 0.169, 0.886).chroma
  val Brown = Srgb(0.647, 0.165, 0.165).chroma
  val BurlyWood = Srgb(0.871, 0.722, 0.529).chroma
  val CadetBlue = Srgb(0.373, 0.62, 0.627).chroma
  val Chartreuse = Srgb(0.498, 1, 0).chroma
  val Chocolate = Srgb(0.824, 0.412, 0.118).chroma
  val Coral = Srgb(1, 0.498, 0.314).chroma
  val CornflowerBlue = Srgb(0.392, 0.584, 0.929).chroma
  val Cornsilk = Srgb(1, 0.973, 0.863).chroma
  val Crimson = Srgb(0.863, 0.078, 0.235).chroma
  val Cyan = Srgb(0, 1, 1).chroma
  val DarkBlue = Srgb(0, 0, 0.545).chroma
  val DarkCyan = Srgb(0, 0.545, 0.545).chroma
  val DarkGoldenrod = Srgb(0.722, 0.525, 0.043).chroma
  val DarkGray = Srgb(0.663, 0.663, 0.663).chroma
  val DarkGreen = Srgb(0, 0.392, 0).chroma
  val DarkKhaki = Srgb(0.741, 0.718, 0.42).chroma
  val DarkMagenta = Srgb(0.545, 0, 0.545).chroma
  val DarkOliveGreen = Srgb(0.333, 0.42, 0.184).chroma
  val DarkOrange = Srgb(1, 0.549, 0).chroma
  val DarkOrchid = Srgb(0.6, 0.196, 0.8).chroma
  val DarkRed = Srgb(0.545, 0, 0).chroma
  val DarkSalmon = Srgb(0.914, 0.588, 0.478).chroma
  val DarkSeaGreen = Srgb(0.561, 0.737, 0.545).chroma
  val DarkSlateBlue = Srgb(0.282, 0.239, 0.545).chroma
  val DarkSlateGray = Srgb(0.184, 0.31, 0.31).chroma
  val DarkTurquoise = Srgb(0, 0.808, 0.82).chroma
  val DarkViolet = Srgb(0.58, 0, 0.827).chroma
  val DeepPink = Srgb(1, 0.078, 0.576).chroma
  val DeepSkyBlue = Srgb(0, 0.749, 1).chroma
  val DimGray = Srgb(0.412, 0.412, 0.412).chroma
  val DodgerBlue = Srgb(0.118, 0.565, 1).chroma
  val FireBrick = Srgb(0.698, 0.133, 0.133).chroma
  val FloralWhite = Srgb(1, 0.98, 0.941).chroma
  val ForestGreen = Srgb(0.133, 0.545, 0.133).chroma
  val Fuchsia = Srgb(1, 0, 1).chroma
  val Gainsboro = Srgb(0.863, 0.863, 0.863).chroma
  val GhostWhite = Srgb(0.973, 0.973, 1).chroma
  val Goldenrod = Srgb(0.855, 0.647, 0.125).chroma
  val Gold = Srgb(1, 0.843, 0).chroma
  val Gray = Srgb(0.502, 0.502, 0.502).chroma
  val Green = Srgb(0, 0.502, 0).chroma
  val GreenYellow = Srgb(0.678, 1, 0.184).chroma
  val HoneyDew = Srgb(0.941, 1, 0.941).chroma
  val HotPink = Srgb(1, 0.412, 0.706).chroma
  val IndianRed = Srgb(0.804, 0.361, 0.361).chroma
  val Indigo = Srgb(0.294, 0, 0.51).chroma
  val Ivory = Srgb(1, 1, 0.941).chroma
  val Khaki = Srgb(0.941, 0.902, 0.549).chroma
  val LavenderBlush = Srgb(1, 0.941, 0.961).chroma
  val Lavender = Srgb(0.902, 0.902, 0.98).chroma
  val LawnGreen = Srgb(0.486, 0.988, 0).chroma
  val LemonChiffon = Srgb(1, 0.98, 0.804).chroma
  val LightBlue = Srgb(0.678, 0.847, 0.902).chroma
  val LightCoral = Srgb(0.941, 0.502, 0.502).chroma
  val LightCyan = Srgb(0.878, 1, 1).chroma
  val LightGoldenrodYellow = Srgb(0.98, 0.98, 0.824).chroma
  val LightGray = Srgb(0.827, 0.827, 0.827).chroma
  val LightGreen = Srgb(0.565, 0.933, 0.565).chroma
  val LightPink = Srgb(1, 0.714, 0.757).chroma
  val LightSalmon = Srgb(1, 0.627, 0.478).chroma
  val LightSeaGreen = Srgb(0.125, 0.698, 0.667).chroma
  val LightSkyBlue = Srgb(0.529, 0.808, 0.98).chroma
  val LightSlateGray = Srgb(0.467, 0.533, 0.6).chroma
  val LightSteelBlue = Srgb(0.69, 0.769, 0.871).chroma
  val LightYellow = Srgb(1, 1, 0.878).chroma
  val LimeGreen = Srgb(0.196, 0.804, 0.196).chroma
  val Lime = Srgb(0, 1, 0).chroma
  val Linen = Srgb(0.98, 0.941, 0.902).chroma
  val Magenta = Srgb(1, 0, 1).chroma
  val Maroon = Srgb(0.502, 0, 0).chroma
  val MediumAquamarine = Srgb(0.4, 0.804, 0.667).chroma
  val MediumBlue = Srgb(0, 0, 0.804).chroma
  val MediumOrchid = Srgb(0.729, 0.333, 0.827).chroma
  val MediumPurple = Srgb(0.576, 0.439, 0.859).chroma
  val MediumSeaGreen = Srgb(0.235, 0.702, 0.443).chroma
  val MediumSlateBlue = Srgb(0.482, 0.408, 0.933).chroma
  val MediumSpringGreen = Srgb(0, 0.98, 0.604).chroma
  val MediumTurquoise = Srgb(0.282, 0.82, 0.8).chroma
  val MediumVioletRed = Srgb(0.78, 0.082, 0.522).chroma
  val MidnightBlue = Srgb(0.098, 0.098, 0.439).chroma
  val MintCream = Srgb(0.961, 1, 0.98).chroma
  val MistyRose = Srgb(1, 0.894, 0.882).chroma
  val Moccasin = Srgb(1, 0.894, 0.71).chroma
  val NavajoWhite = Srgb(1, 0.871, 0.678).chroma
  val Navy = Srgb(0, 0, 0.502).chroma
  val OldLace = Srgb(0.992, 0.961, 0.902).chroma
  val OliveDrab = Srgb(0.42, 0.557, 0.137).chroma
  val Olive = Srgb(0.502, 0.502, 0).chroma
  val OrangeRed = Srgb(1, 0.271, 0).chroma
  val Orange = Srgb(1, 0.647, 0).chroma
  val Orchid = Srgb(0.855, 0.439, 0.839).chroma
  val PaleGoldenrod = Srgb(0.933, 0.91, 0.667).chroma
  val PaleGreen = Srgb(0.596, 0.984, 0.596).chroma
  val PaleTurquoise = Srgb(0.686, 0.933, 0.933).chroma
  val PaleVioletRed = Srgb(0.859, 0.439, 0.576).chroma
  val PapayaWhip = Srgb(1, 0.937, 0.835).chroma
  val PeachPuff = Srgb(1, 0.855, 0.725).chroma
  val Peru = Srgb(0.804, 0.522, 0.247).chroma
  val Pink = Srgb(1, 0.753, 0.796).chroma
  val Plum = Srgb(0.867, 0.627, 0.867).chroma
  val PowderBlue = Srgb(0.69, 0.878, 0.902).chroma
  val Purple = Srgb(0.502, 0, 0.502).chroma
  val RebeccaPurple = Srgb(0.4, 0.2, 0.6).chroma
  val Red = Srgb(1, 0, 0).chroma
  val RosyBrown = Srgb(0.737, 0.561, 0.561).chroma
  val RoyalBlue = Srgb(0.255, 0.412, 0.882).chroma
  val SaddleBrown = Srgb(0.545, 0.271, 0.075).chroma
  val Salmon = Srgb(0.98, 0.502, 0.447).chroma
  val SandyBrown = Srgb(0.957, 0.643, 0.376).chroma
  val SeaGreen = Srgb(0.18, 0.545, 0.341).chroma
  val SeaShell = Srgb(1, 0.961, 0.933).chroma
  val Sienna = Srgb(0.627, 0.322, 0.176).chroma
  val Silver = Srgb(0.753, 0.753, 0.753).chroma
  val SkyBlue = Srgb(0.529, 0.808, 0.922).chroma
  val SlateBlue = Srgb(0.416, 0.353, 0.804).chroma
  val SlateGray = Srgb(0.439, 0.502, 0.565).chroma
  val Snow = Srgb(1, 0.98, 0.98).chroma
  val SpringGreen = Srgb(0, 1, 0.498).chroma
  val SteelBlue = Srgb(0.275, 0.51, 0.706).chroma
  val Tan = Srgb(0.824, 0.706, 0.549).chroma
  val Teal = Srgb(0, 0.502, 0.502).chroma
  val Thistle = Srgb(0.847, 0.749, 0.847).chroma
  val Tomato = Srgb(1, 0.388, 0.278).chroma
  val Turquoise = Srgb(0.251, 0.878, 0.816).chroma
  val Violet = Srgb(0.933, 0.51, 0.933).chroma
  val Wheat = Srgb(0.961, 0.871, 0.702).chroma
  val WhiteSmoke = Srgb(0.961, 0.961, 0.961).chroma
  val White = Srgb(1, 1, 1).chroma
  val YellowGreen = Srgb(0.604, 0.804, 0.196).chroma
  val Yellow = Srgb(1, 1, 0).chroma

  val all = List(AliceBlue, AntiqueWhite, Aquamarine, Aqua, Azure, Beige, Bisque, Black,
      BlanchedAlmond, Blue, BlueViolet, Brown, BurlyWood, CadetBlue, Chartreuse, Chocolate, Coral,
      CornflowerBlue, Cornsilk, Crimson, Cyan, DarkBlue, DarkCyan, DarkGoldenrod, DarkGray,
      DarkGreen, DarkKhaki, DarkMagenta, DarkOliveGreen, DarkOrange, DarkOrchid, DarkRed,
      DarkSalmon, DarkSeaGreen, DarkSlateBlue, DarkSlateGray, DarkTurquoise, DarkViolet, DeepPink,
      DeepSkyBlue, DimGray, DodgerBlue, FireBrick, FloralWhite, ForestGreen, Fuchsia, Gainsboro,
      GhostWhite, Goldenrod, Gold, Gray, Green, GreenYellow, HoneyDew, HotPink, IndianRed, Indigo,
      Ivory, Khaki, LavenderBlush, Lavender, LawnGreen, LemonChiffon, LightBlue, LightCoral,
      LightCyan, LightGoldenrodYellow, LightGray, LightGreen, LightPink, LightSalmon, LightSeaGreen,
      LightSkyBlue, LightSlateGray, LightSteelBlue, LightYellow, LimeGreen, Lime, Linen, Magenta,
      Maroon, MediumAquamarine, MediumBlue, MediumOrchid, MediumPurple, MediumSeaGreen,
      MediumSlateBlue, MediumSpringGreen, MediumTurquoise, MediumVioletRed, MidnightBlue, MintCream,
      MistyRose, Moccasin, NavajoWhite, Navy, OldLace, OliveDrab, Olive, OrangeRed, Orange, Orchid,
      PaleGoldenrod, PaleGreen, PaleTurquoise, PaleVioletRed, PapayaWhip, PeachPuff, Peru, Pink,
      Plum, PowderBlue, Purple, RebeccaPurple, Red, RosyBrown, RoyalBlue, SaddleBrown, Salmon,
      SandyBrown, SeaGreen, SeaShell, Sienna, Silver, SkyBlue, SlateBlue, SlateGray, Snow,
      SpringGreen, SteelBlue, Tan, Teal, Thistle, Tomato, Turquoise, Violet, Wheat, WhiteSmoke,
      White, YellowGreen, Yellow)
