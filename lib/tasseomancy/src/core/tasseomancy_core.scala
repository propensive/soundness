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
package tasseomancy

import gossamer.*
import iridescence.*
import prepositional.*
import quantitative.*
import symbolism.*

// The entry point: data and a chart kind, checked for compatibility as the code compiles, become
// a chart fitted to the data.
extension [data](data: data)
  def chart[form, fit](form: form)(using plottable: data is Plottable in form to fit)
  :   Chart[data, form, fit] =

    Chart(data, form, plottable.fit(form, data))

package calibrations:
  given linearCalibration: [value] => value is Calibration = Calibration(Calibration.Policy.Linear)

  given logarithmicCalibration: [value] => value is Calibration =
    Calibration(Calibration.Policy.Logarithmic)

  given adaptiveCalibration: [value] => value is Calibration =
    Calibration(Calibration.Policy.Adaptive)

  given tightCalibration: [value] => value is Calibration = Calibration(Calibration.Policy.Tight)

package palettes:
  private def rgb(red: Double, green: Double, blue: Double): Color in Srgb = Srgb(red, green, blue)

  given slateChartPalette: ChartPalette = new ChartPalette:
    val series: Sequence[Color in Srgb] =
      Sequence
        ( rgb(0.306, 0.475, 0.655), rgb(0.949, 0.557, 0.169), rgb(0.882, 0.341, 0.349),
          rgb(0.463, 0.718, 0.698), rgb(0.349, 0.631, 0.310), rgb(0.929, 0.788, 0.282),
          rgb(0.690, 0.478, 0.631), rgb(1.000, 0.616, 0.655) )

    def background: Color in Srgb = rgb(1.0, 1.0, 1.0)
    def foreground: Color in Srgb = rgb(0.2, 0.2, 0.2)
    def axis: Color in Srgb = rgb(0.4, 0.4, 0.4)
    def grid: Color in Srgb = rgb(0.87, 0.87, 0.87)
    def text: Color in Srgb = rgb(0.2, 0.2, 0.2)

  private val solarizedSeries: Sequence[Color in Srgb] =
    Sequence
      ( rgb(0.149, 0.545, 0.824), rgb(0.796, 0.294, 0.086), rgb(0.522, 0.600, 0.000),
        rgb(0.827, 0.212, 0.510), rgb(0.165, 0.631, 0.596), rgb(0.710, 0.537, 0.000),
        rgb(0.424, 0.443, 0.769), rgb(0.863, 0.196, 0.184) )

  given solarizedDarkChartPalette: ChartPalette = new ChartPalette:
    val series: Sequence[Color in Srgb] = solarizedSeries
    def background: Color in Srgb = rgb(0.000, 0.169, 0.212)
    def foreground: Color in Srgb = rgb(0.514, 0.580, 0.588)
    def axis: Color in Srgb = rgb(0.345, 0.431, 0.459)
    def grid: Color in Srgb = rgb(0.027, 0.212, 0.259)
    def text: Color in Srgb = rgb(0.576, 0.631, 0.631)

  given solarizedLightChartPalette: ChartPalette = new ChartPalette:
    val series: Sequence[Color in Srgb] = solarizedSeries
    def background: Color in Srgb = rgb(0.992, 0.965, 0.890)
    def foreground: Color in Srgb = rgb(0.396, 0.482, 0.514)
    def axis: Color in Srgb = rgb(0.576, 0.631, 0.631)
    def grid: Color in Srgb = rgb(0.933, 0.910, 0.835)
    def text: Color in Srgb = rgb(0.345, 0.431, 0.459)

package fontMetrics:
  // Six tenths of an em per character: the width of an average proportional face.
  given averageFontMetric: FontMetric = text => 0.6*text.length*Em

  // Half an em per character: a monospace face.
  given tabularFontMetric: FontMetric = text => 0.5*text.length*Em
