/*
    Aviation, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package aviation

import spectacular.*
import gossamer.*
import hieroglyph.*, textMetrics.uniform

object Clockface:
  given Clockface is Showable = clockface =>
    val hour = (clockface.hour: Int).show.pad(2, Bidi.Rtl, '0')
    val minute = (clockface.minute: Int).show.pad(2, Bidi.Rtl, '0')

    t"$hour:$minute"

case class Clockface(hour: Base24, minute: Base60, second: Base60 = 0)
