/*
    Cataclysm, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cataclysm

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*

case class FontFace
   (ascentOverride:     Optional[Text] = Unset,
    descentOverride:      Optional[Text] = Unset,
    fontDisplay:        Optional[Text] = Unset,
    fontFamily:         Optional[Text] = Unset,
    fontStretch:        Optional[Text] = Unset,
    fontStyle:          Optional[Text] = Unset,
    fontWeight:         Optional[Text] = Unset,
    fontVariationSettings: Optional[Text] = Unset,
    lineGapOverride:      Optional[Text] = Unset,
    sizeAdjust:         Optional[Text] = Unset,
    src:                Optional[Text] = Unset,
    unicodeRange:       Optional[Text] = Unset)
extends CssStylesheet.Item:

  def text: Text =
    val params = List(
      t"ascent-override"         -> ascentOverride,
      t"descent-override"        -> descentOverride,
      t"font-display"            -> fontDisplay,
      t"font-family"             -> fontFamily,
      t"font-weight"             -> fontWeight,
      t"font-variation-settings" -> fontVariationSettings,
      t"line-gap-override"       -> lineGapOverride,
      t"size-adjust"             -> sizeAdjust,
      t"src"                     -> src,
      t"unicode-range"           -> unicodeRange
    ).filter(!_(1).absent)

    params.collect:
      case (key: Text, value: Text) => t"$key: $value;"

    . join(t"@font-face { ", t" ", t" }")
