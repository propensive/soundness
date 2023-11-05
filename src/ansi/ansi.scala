package eucalyptus

import escapade.*
import iridescence.*
import anticipation.*
import gossamer.*
import spectacular.*
import hieroglyph.textWidthCalculation.uniform

package logFormats:
  given standardColor[TargetType]: LogFormat[TargetType] = entry =>
    given displayLevel: Displayable[Level] = level =>
      val color = level match
      case Level.Fine => colors.LightSeaGreen
      case Level.Info => colors.YellowGreen
      case Level.Warn => colors.Gold
      case Level.Fail => colors.Tomato

      e"${Bg(color)}[${colors.Black}($Bold( ${level.show.upper} ))]"

    val realm: Output = e"${colors.MediumSeaGreen}(${entry.realm.show.fit(8)})"

    e"${colors.SlateGray}(${Log.dateFormat.format(entry.timestamp).nn.tt}) ${entry.level} $realm ${entry.message}\n".render