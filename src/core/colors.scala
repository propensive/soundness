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

object colors:
  val AliceBlue = Srgb(0.941, 0.973, 1).rgb24
  val AntiqueWhite = Srgb(0.98, 0.922, 0.843).rgb24
  val Aquamarine = Srgb(0.498, 1, 0.831).rgb24
  val Aqua = Srgb(0, 1, 1).rgb24
  val Azure = Srgb(0.941, 1, 1).rgb24
  val Beige = Srgb(0.961, 0.961, 0.863).rgb24
  val Bisque = Srgb(1, 0.894, 0.769).rgb24
  val Black = Srgb(0, 0, 0).rgb24
  val BlanchedAlmond = Srgb(1, 0.922, 0.804).rgb24
  val Blue = Srgb(0, 0, 1).rgb24
  val BlueViolet = Srgb(0.541, 0.169, 0.886).rgb24
  val Brown = Srgb(0.647, 0.165, 0.165).rgb24
  val BurlyWood = Srgb(0.871, 0.722, 0.529).rgb24
  val CadetBlue = Srgb(0.373, 0.62, 0.627).rgb24
  val Chartreuse = Srgb(0.498, 1, 0).rgb24
  val Chocolate = Srgb(0.824, 0.412, 0.118).rgb24
  val Coral = Srgb(1, 0.498, 0.314).rgb24
  val CornflowerBlue = Srgb(0.392, 0.584, 0.929).rgb24
  val Cornsilk = Srgb(1, 0.973, 0.863).rgb24
  val Crimson = Srgb(0.863, 0.078, 0.235).rgb24
  val Cyan = Srgb(0, 1, 1).rgb24
  val DarkBlue = Srgb(0, 0, 0.545).rgb24
  val DarkCyan = Srgb(0, 0.545, 0.545).rgb24
  val DarkGoldenrod = Srgb(0.722, 0.525, 0.043).rgb24
  val DarkGray = Srgb(0.663, 0.663, 0.663).rgb24
  val DarkGreen = Srgb(0, 0.392, 0).rgb24
  val DarkKhaki = Srgb(0.741, 0.718, 0.42).rgb24
  val DarkMagenta = Srgb(0.545, 0, 0.545).rgb24
  val DarkOliveGreen = Srgb(0.333, 0.42, 0.184).rgb24
  val DarkOrange = Srgb(1, 0.549, 0).rgb24
  val DarkOrchid = Srgb(0.6, 0.196, 0.8).rgb24
  val DarkRed = Srgb(0.545, 0, 0).rgb24
  val DarkSalmon = Srgb(0.914, 0.588, 0.478).rgb24
  val DarkSeaGreen = Srgb(0.561, 0.737, 0.545).rgb24
  val DarkSlateBlue = Srgb(0.282, 0.239, 0.545).rgb24
  val DarkSlateGray = Srgb(0.184, 0.31, 0.31).rgb24
  val DarkTurquoise = Srgb(0, 0.808, 0.82).rgb24
  val DarkViolet = Srgb(0.58, 0, 0.827).rgb24
  val DeepPink = Srgb(1, 0.078, 0.576).rgb24
  val DeepSkyBlue = Srgb(0, 0.749, 1).rgb24
  val DimGray = Srgb(0.412, 0.412, 0.412).rgb24
  val DodgerBlue = Srgb(0.118, 0.565, 1).rgb24
  val FireBrick = Srgb(0.698, 0.133, 0.133).rgb24
  val FloralWhite = Srgb(1, 0.98, 0.941).rgb24
  val ForestGreen = Srgb(0.133, 0.545, 0.133).rgb24
  val Fuchsia = Srgb(1, 0, 1).rgb24
  val Gainsboro = Srgb(0.863, 0.863, 0.863).rgb24
  val GhostWhite = Srgb(0.973, 0.973, 1).rgb24
  val Goldenrod = Srgb(0.855, 0.647, 0.125).rgb24
  val Gold = Srgb(1, 0.843, 0).rgb24
  val Gray = Srgb(0.502, 0.502, 0.502).rgb24
  val Green = Srgb(0, 0.502, 0).rgb24
  val GreenYellow = Srgb(0.678, 1, 0.184).rgb24
  val HoneyDew = Srgb(0.941, 1, 0.941).rgb24
  val HotPink = Srgb(1, 0.412, 0.706).rgb24
  val IndianRed = Srgb(0.804, 0.361, 0.361).rgb24
  val Indigo = Srgb(0.294, 0, 0.51).rgb24
  val Ivory = Srgb(1, 1, 0.941).rgb24
  val Khaki = Srgb(0.941, 0.902, 0.549).rgb24
  val LavenderBlush = Srgb(1, 0.941, 0.961).rgb24
  val Lavender = Srgb(0.902, 0.902, 0.98).rgb24
  val LawnGreen = Srgb(0.486, 0.988, 0).rgb24
  val LemonChiffon = Srgb(1, 0.98, 0.804).rgb24
  val LightBlue = Srgb(0.678, 0.847, 0.902).rgb24
  val LightCoral = Srgb(0.941, 0.502, 0.502).rgb24
  val LightCyan = Srgb(0.878, 1, 1).rgb24
  val LightGoldenrodYellow = Srgb(0.98, 0.98, 0.824).rgb24
  val LightGray = Srgb(0.827, 0.827, 0.827).rgb24
  val LightGreen = Srgb(0.565, 0.933, 0.565).rgb24
  val LightPink = Srgb(1, 0.714, 0.757).rgb24
  val LightSalmon = Srgb(1, 0.627, 0.478).rgb24
  val LightSeaGreen = Srgb(0.125, 0.698, 0.667).rgb24
  val LightSkyBlue = Srgb(0.529, 0.808, 0.98).rgb24
  val LightSlateGray = Srgb(0.467, 0.533, 0.6).rgb24
  val LightSteelBlue = Srgb(0.69, 0.769, 0.871).rgb24
  val LightYellow = Srgb(1, 1, 0.878).rgb24
  val LimeGreen = Srgb(0.196, 0.804, 0.196).rgb24
  val Lime = Srgb(0, 1, 0).rgb24
  val Linen = Srgb(0.98, 0.941, 0.902).rgb24
  val Magenta = Srgb(1, 0, 1).rgb24
  val Maroon = Srgb(0.502, 0, 0).rgb24
  val MediumAquamarine = Srgb(0.4, 0.804, 0.667).rgb24
  val MediumBlue = Srgb(0, 0, 0.804).rgb24
  val MediumOrchid = Srgb(0.729, 0.333, 0.827).rgb24
  val MediumPurple = Srgb(0.576, 0.439, 0.859).rgb24
  val MediumSeaGreen = Srgb(0.235, 0.702, 0.443).rgb24
  val MediumSlateBlue = Srgb(0.482, 0.408, 0.933).rgb24
  val MediumSpringGreen = Srgb(0, 0.98, 0.604).rgb24
  val MediumTurquoise = Srgb(0.282, 0.82, 0.8).rgb24
  val MediumVioletRed = Srgb(0.78, 0.082, 0.522).rgb24
  val MidnightBlue = Srgb(0.098, 0.098, 0.439).rgb24
  val MintCream = Srgb(0.961, 1, 0.98).rgb24
  val MistyRose = Srgb(1, 0.894, 0.882).rgb24
  val Moccasin = Srgb(1, 0.894, 0.71).rgb24
  val NavajoWhite = Srgb(1, 0.871, 0.678).rgb24
  val Navy = Srgb(0, 0, 0.502).rgb24
  val OldLace = Srgb(0.992, 0.961, 0.902).rgb24
  val OliveDrab = Srgb(0.42, 0.557, 0.137).rgb24
  val Olive = Srgb(0.502, 0.502, 0).rgb24
  val OrangeRed = Srgb(1, 0.271, 0).rgb24
  val Orange = Srgb(1, 0.647, 0).rgb24
  val Orchid = Srgb(0.855, 0.439, 0.839).rgb24
  val PaleGoldenrod = Srgb(0.933, 0.91, 0.667).rgb24
  val PaleGreen = Srgb(0.596, 0.984, 0.596).rgb24
  val PaleTurquoise = Srgb(0.686, 0.933, 0.933).rgb24
  val PaleVioletRed = Srgb(0.859, 0.439, 0.576).rgb24
  val PapayaWhip = Srgb(1, 0.937, 0.835).rgb24
  val PeachPuff = Srgb(1, 0.855, 0.725).rgb24
  val Peru = Srgb(0.804, 0.522, 0.247).rgb24
  val Pink = Srgb(1, 0.753, 0.796).rgb24
  val Plum = Srgb(0.867, 0.627, 0.867).rgb24
  val PowderBlue = Srgb(0.69, 0.878, 0.902).rgb24
  val Purple = Srgb(0.502, 0, 0.502).rgb24
  val RebeccaPurple = Srgb(0.4, 0.2, 0.6).rgb24
  val Red = Srgb(1, 0, 0).rgb24
  val RosyBrown = Srgb(0.737, 0.561, 0.561).rgb24
  val RoyalBlue = Srgb(0.255, 0.412, 0.882).rgb24
  val SaddleBrown = Srgb(0.545, 0.271, 0.075).rgb24
  val Salmon = Srgb(0.98, 0.502, 0.447).rgb24
  val SandyBrown = Srgb(0.957, 0.643, 0.376).rgb24
  val SeaGreen = Srgb(0.18, 0.545, 0.341).rgb24
  val SeaShell = Srgb(1, 0.961, 0.933).rgb24
  val Sienna = Srgb(0.627, 0.322, 0.176).rgb24
  val Silver = Srgb(0.753, 0.753, 0.753).rgb24
  val SkyBlue = Srgb(0.529, 0.808, 0.922).rgb24
  val SlateBlue = Srgb(0.416, 0.353, 0.804).rgb24
  val SlateGray = Srgb(0.439, 0.502, 0.565).rgb24
  val Snow = Srgb(1, 0.98, 0.98).rgb24
  val SpringGreen = Srgb(0, 1, 0.498).rgb24
  val SteelBlue = Srgb(0.275, 0.51, 0.706).rgb24
  val Tan = Srgb(0.824, 0.706, 0.549).rgb24
  val Teal = Srgb(0, 0.502, 0.502).rgb24
  val Thistle = Srgb(0.847, 0.749, 0.847).rgb24
  val Tomato = Srgb(1, 0.388, 0.278).rgb24
  val Turquoise = Srgb(0.251, 0.878, 0.816).rgb24
  val Violet = Srgb(0.933, 0.51, 0.933).rgb24
  val Wheat = Srgb(0.961, 0.871, 0.702).rgb24
  val WhiteSmoke = Srgb(0.961, 0.961, 0.961).rgb24
  val White = Srgb(1, 1, 1).rgb24
  val YellowGreen = Srgb(0.604, 0.804, 0.196).rgb24
  val Yellow = Srgb(1, 1, 0).rgb24

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

object solarized:
  val Base03  = Srgb(0.000, 0.169, 0.212)
  val Base02  = Srgb(0.027, 0.212, 0.259)
  val Base01  = Srgb(0.345, 0.431, 0.459)
  val Base00  = Srgb(0.396, 0.482, 0.514)
  val Base0   = Srgb(0.514, 0.580, 0.588)
  val Base1   = Srgb(0.576, 0.631, 0.631)
  val Base2   = Srgb(0.933, 0.910, 0.835)
  val Base3   = Srgb(0.992, 0.965, 0.890)
  val Yellow  = Srgb(0.710, 0.537, 0.000)
  val Orange  = Srgb(0.796, 0.294, 0.086)
  val Red     = Srgb(0.863, 0.196, 0.184)
  val Magenta = Srgb(0.827, 0.212, 0.510)
  val Violet  = Srgb(0.424, 0.443, 0.769)
  val Blue    = Srgb(0.149, 0.545, 0.824)
  val Cyan    = Srgb(0.165, 0.631, 0.596)
  val Green   = Srgb(0.522, 0.600, 0.000)