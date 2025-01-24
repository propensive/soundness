/*
    Iridescence, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import scala.util.chaining.*

import anticipation.*
import contextual.*
import fulminate.*
import rudiments.*

import errorDiagnostics.empty

object RgbHex extends Interpolator[Nothing, Option[Rgb24], Rgb24]:
  def initial: Option[Rgb24] = None

  def parse(state: Option[Rgb24], next: Text): Option[Rgb24] =
    if next.s.length == 7 && next.s.startsWith("#") then parse(state, Text(next.s.substring(1).nn))
    else if next.s.length == 6 && next.s.all: char =>
      char.isDigit || ((char | 32) >= 'a' && (char | 32) <= 'f')
    then
      val red = Integer.parseInt(next.s.substring(0, 2).nn, 16)
      val green = Integer.parseInt(next.s.substring(2, 4).nn, 16)
      val blue = Integer.parseInt(next.s.substring(4, 6).nn, 16)

      Some(Rgb24(red, green, blue))

    else throw InterpolationError(m"""the color must be in the form ${Text("rgb\"#rrggbb\"")} or
        rgb"rrggbb" where rr, gg and bb are 2-digit hex values""", 0)

  def insert(state: Option[Rgb24], value: Nothing): Option[Rgb24] =
    throw InterpolationError:
      m"substitutions into an ${Text("rgb\"\"")} interpolator are not supported"

  def skip(state: Option[Rgb24]): Option[Rgb24] = state
  def complete(color: Option[Rgb24]): Rgb24 = color.get
