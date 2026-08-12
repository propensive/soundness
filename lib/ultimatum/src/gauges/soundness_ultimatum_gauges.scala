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
  ultimatum
  . { Bar, Byte, Bytes, CaptionLayout, Captioned, Checklist, Countdown, Dial, Facet,
      Fraction, gauge, Gaugeable, GaugePalette, gaugeLine, gaugeRows, Gauging, Gradient,
      Information, Inlay, Magnitude, Meter, Reading, Reckoning, Sparkline,
      Spinner, Standing, Step, Stopwatch, Transfer, whilst }

// Every given is exported by name: a wildcard would silently drop them, since `import p.*` does not
// import givens.

package spinners:
  export
    ultimatum.spinners
    . { aestheticSpinner, arcSpinner, arrowDoubleSpinner, arrowSpinner, balloonSpinner,
        binarySpinner, bounceSpinner, bouncingBallSpinner, bouncingBarSpinner, boxSpinner,
        brailleDotsSpinner, brailleGrowSpinner, brailleSnakeSpinner, brailleWaveSpinner,
        circleHalfSpinner, circlePulseSpinner, circleQuadrantSpinner, clockSpinner,
        crossStarSpinner, dotsScrollSpinner, dqpbSpinner, earthSpinner, growingBarSpinner,
        growingBlockSpinner, hamburgerSpinner, hourglassSpinner, hourglassThinSpinner, layerSpinner,
        lineSpinner, moonPhaseSpinner, noiseSpinner, pipeSpinner, pointsSpinner, pulseSpinner,
        shuttleSpinner, squareCornerSpinner, starSpinner, toggleRoundSpinner, toggleSpinner,
        toggleSquareSpinner, triangleSpinner }

package bars:
  export
    ultimatum.bars
    . { arrowheadBar, asciiBar, blockBar, brailleBar, capsuleBar, dotBar, equalsBar, fineBar,
        gradientBar, markerBar, percentageBar, pipBar, railBar, risingBar, segmentedBar, shadedBar,
        smoothBar, squareBar }

package meters:
  export
    ultimatum.meters
    . { asciiMeter, batteryMeter, bulletMeter, columnMeter, needleMeter, thermometerMeter }

package sparklines:
  export ultimatum.sparklines.{asciiSparkline, blockSparkline, dotSparkline, tallSparkline}

package counters:
  export
    ultimatum.counters
    . { decimalTransferCounter, paddedCounter, percentageCounter, plainCounter, rateTransferCounter,
        scaledCounter, terseTransferCounter, transferCounter, wordCounter }

package standings:
  export
    ultimatum.standings
    . { asciiStanding, heavyStanding, squareStanding, tickStanding, wordStanding }

package processions:
  export
    ultimatum.processions
    . { beadProcession, breadcrumbProcession, checklistProcession, numberedProcession,
        ribbonProcession }

package palettes:
  export
    ultimatum.palettes
    . { ansiSixteenGaugePalette, emberGaugePalette, monochromeGaugePalette, oceanicGaugePalette,
        plumGaugePalette, signalGaugePalette, slateGaugePalette, solarizedDarkGaugePalette,
        solarizedLightGaugePalette, verdantGaugePalette }

package timers:
  export
    ultimatum.timers
    . { compactCountdown, compactElapsed, digitalCountdown, digitalElapsed, urgentCountdown }

package captions:
  export ultimatum.captions.{leadingCaption, spacedCaption, trailingCaption, truncatedCaption}

package gaugeGlyphs:
  export ultimatum.gaugeGlyphs.{asciiGlyphs, brailleGlyphs, emojiGlyphs, unicodeGlyphs}

package informationPrefixes:
  export ultimatum.informationPrefixes.{binaryBytes, decimalBytes}
