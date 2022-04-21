/*
    Escapade, version 0.4.0. Copyright 2021-22 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package escapade

import rudiments.*

extension (inline ctx: StringContext)
  transparent inline def ansi(inline parts: Any*): AnsiText =
    ${Ansi.Interpolator.expand('{Ansi.Interpolator}, 'ctx, 'parts)}

extension [T](value: T)
  def ansi(using AnsiShow[T]): AnsiText = summon[AnsiShow[T]].ansiShow(value)
