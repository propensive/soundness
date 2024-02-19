/*
    Eucalyptus, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package eucalyptus

import escapade.*
import iridescence.*
import anticipation.*
import gossamer.*
import spectacular.*
import hieroglyph.textMetrics.uniform

//import scala.language.experimental.captureChecking

package logFormats:
  given standardColor[TargetType]: LogFormat[TargetType, Output] = entry =>
    given displayLevel: Displayable[Level] = level =>
      val color = level match
        case Level.Fine => colors.LightSeaGreen
        case Level.Info => colors.YellowGreen
        case Level.Warn => colors.Gold
        case Level.Fail => colors.Tomato

      e"${Bg(color)}[${colors.Black}($Bold( ${level.show} ))]"

    // FIXME: This is far too much object creation for every log message
    val realm: Output = e"${colors.SteelBlue}(${entry.realm.name.fit(8)})"
    val colorSeq = List(colors.Chocolate, colors.OliveDrab, colors.CadetBlue, colors.DarkGoldenrod)
    
    val stack: Output =
      entry.envelopes.reverse.zip(colorSeq).map { (item, color) => e"$color($item)" }.join(e"", e" ⟩ ", e" ⟩")
    
    val dateTime = Log.dateFormat.format(entry.timestamp).nn.tt

    e"${colors.SlateGray}($dateTime) ${entry.level} $realm $stack ${entry.message}\n"
