/*
    Escapade, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import gossamer.*
import spectacular.*
import vacuous.*

import language.experimental.captureChecking

object csi:
  val esc: Text = t"\e"
  val csi: Text = t"\e["
  def cuu(n: Optional[Int] = Unset): Text = t"$csi${n.let(_.show).or(t"")}A"
  def cud(n: Optional[Int] = Unset): Text = t"$csi${n.let(_.show).or(t"")}B"
  def cuf(n: Optional[Int] = Unset): Text = t"$csi${n.let(_.show).or(t"")}C"
  def cub(n: Optional[Int] = Unset): Text = t"$csi${n.let(_.show).or(t"")}D"
  def cnl(n: Optional[Int] = Unset): Text = t"$csi${n.let(_.show).or(t"")}E"
  def cpl(n: Optional[Int] = Unset): Text = t"$csi${n.let(_.show).or(t"")}F"
  def cha(n: Optional[Int] = Unset): Text = t"$csi${n.let(_.show).or(t"")}G"
  def cup(n: Optional[Int] = Unset, m: Optional[Int] = Unset): Text = t"$csi${n.let(_.show).or(t"")};${m.let(_.show).or(t"")}H"
  def ed(n: Optional[Int] = Unset): Text = t"$csi${n.let(_.show).or(t"")}J"
  def el(n: Optional[Int] = Unset): Text = t"$csi${n.let(_.show).or(t"")}K"
  def su(n: Optional[Int] = Unset): Text = t"$csi${n.let(_.show).or(t"")}S"
  def sd(n: Optional[Int] = Unset): Text = t"$csi${n.let(_.show).or(t"")}T"
  def hvp(n: Int, m: Int): Text = t"$csi${n};${m}f"
  def dsr(): Text = t"${csi}6n"
  def scp(): Text = t"${csi}s"
  def rcp(): Text = t"${csi}u"
  def sgr(ns: Int*): Text = t"${csi}${ns.map(_.show).join(t";")}m"
  def dectcem(on: Boolean = false): Text = t"${csi}?25${if on then t"h" else t"l"}"
