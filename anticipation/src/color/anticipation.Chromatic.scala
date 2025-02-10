/*
    Anticipation, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package anticipation

import language.experimental.captureChecking

trait Chromatic:
  type Self
  extension (color: Self) def asRgb24Int: Int

  def red(color: Self): Int = asRgb24Int(color)&255
  def green(color: Self): Int = (asRgb24Int(color) >> 8)&255
  def blue(color: Self): Int = (asRgb24Int(color) >> 16)&255
