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
import fulminate.*
import gossamer.*
import rudiments.*
import spectacular.*

given Realm = realm"cataclysm"

private[cataclysm] type Label = String & Singleton

given Decimalizer as decimalizer = Decimalizer(6)

def select[SelectorType: Selectable](sel: SelectorType)(css: CssStyle) =
  CssRule(SelectorType.selector(sel), css)

extension [SelectorType: Selectable](left: SelectorType)
  @targetName("descendant")
  infix def >> [SelectorType2: Selectable](right: SelectorType2): Selector =
    SelectorType.selector(left) >> SelectorType2.selector(right)

  @targetName("after")
  infix def ~ [SelectorType2: Selectable](right: SelectorType2): Selector =
    SelectorType.selector(left) + SelectorType2.selector(right)

  @targetName("or")
  infix def || [SelectorType2: Selectable](right: SelectorType2): Selector =
    SelectorType.selector(left) | SelectorType2.selector(right)

  @targetName("and")
  infix def && [SelectorType2: Selectable](right: SelectorType2): Selector =
    SelectorType.selector(left) & SelectorType2.selector(right)

  @targetName("before")
  infix def ~~ [SelectorType2: Selectable](right: SelectorType2): Selector =
    SelectorType.selector(left) ~ SelectorType2.selector(right)

type Dimension = Length | Int

def max(head: Length, tail: Length*): Length = tail.foldLeft(head)(_.function(t"max", _))
def min(head: Length, tail: Length*): Length = tail.foldLeft(head)(_.function(t"min", _))

package pseudo:
  def dir(direction: Dir) = Selector.PseudoClass(t"dir(${direction.show.lower})")

  def has[SelectorType: Selectable](selector: SelectorType) =
     Selector.PseudoClass(t"has(${SelectorType.selector(selector).value})")

  def webkitScrollbar = Selector.PseudoClass(t":-webkit-scrollbar")
  def lang(language: Text) = Selector.PseudoClass(t"lang($language)")
  val after = Selector.PseudoClass(t":after")
  val before = Selector.PseudoClass(t":before")
  val selection = Selector.PseudoClass(t":selection")
  val firstLetter = Selector.PseudoClass(t":first-letter")
  val firstLine = Selector.PseudoClass(t":first-line")
  val marker = Selector.PseudoClass(t":marker")
  val placeholder = Selector.PseudoClass(t":placeholder")
  val anyLink = Selector.PseudoClass(t"any-link")
  val link = Selector.PseudoClass(t"link")
  val visited = Selector.PseudoClass(t"visited")
  val localLink = Selector.PseudoClass(t"local-link")
  val target = Selector.PseudoClass(t"target")
  val targetWithin = Selector.PseudoClass(t"target-within")
  val scope = Selector.PseudoClass(t"scope")
  val hover = Selector.PseudoClass(t"hover")
  val active = Selector.PseudoClass(t"active")
  val focus = Selector.PseudoClass(t"focus")
  val focusVisible = Selector.PseudoClass(t"focus-visible")
  val focusWithin = Selector.PseudoClass(t"focus-within")
  val current = Selector.PseudoClass(t"current")
  val past = Selector.PseudoClass(t"past")
  val future = Selector.PseudoClass(t"future")
  val playing = Selector.PseudoClass(t"playing")
  val paused = Selector.PseudoClass(t"paused")
  val autofill = Selector.PseudoClass(t"autofill")
  val enabled = Selector.PseudoClass(t"enabled")
  val disabled = Selector.PseudoClass(t"disabled")
  val readOnly = Selector.PseudoClass(t"read-only")
  val readWrite = Selector.PseudoClass(t"read-write")
  val placeholderShown = Selector.PseudoClass(t"placeholder-shown")
  val default = Selector.PseudoClass(t"default")
  val checked = Selector.PseudoClass(t"checked")
  val indeterminate = Selector.PseudoClass(t"indeterminate")
  val blank = Selector.PseudoClass(t"blank")
  val valid = Selector.PseudoClass(t"valid")
  val invalid = Selector.PseudoClass(t"invalid")
  val inRange = Selector.PseudoClass(t"in-range")
  val outOfRange = Selector.PseudoClass(t"out-of-range")
  val required = Selector.PseudoClass(t"required")
  val optional = Selector.PseudoClass(t"option")
  val userInvalid = Selector.PseudoClass(t"user-invalid")
  val root = Selector.PseudoClass(t"root")
  val empty = Selector.PseudoClass(t"empty")

  private def expr(a: Int, b: Int): Text =
    if a == 0 then t"$b" else if b != 0 then t"${a}n+$b" else t"${a}n"

  def nthChild(a: Int, b: Int) = Selector.PseudoClass(t"nth-child(${expr(a, b)})")
  def nthLastChild(a: Int, b: Int) = Selector.PseudoClass(t"nth-last-child(${expr(a, b)})")
  def nthOfType(a: Int, b: Int) = Selector.PseudoClass(t"nth-of-type(${expr(a, b)})")
  def nthLastOfType(a: Int, b: Int) = Selector.PseudoClass(t"nth-last-of-type(${expr(a, b)})")

  val firstChild = Selector.PseudoClass(t"first-child")
  val lastChild = Selector.PseudoClass(t"last-child")
  val onlyChild = Selector.PseudoClass(t"only-child")
  val firstOfType = Selector.PseudoClass(t"first-of-type")
  val lastOfType = Selector.PseudoClass(t"last-of-type")
  val onlyOfType = Selector.PseudoClass(t"only-of-type")

extension (value: Double)
  def sec: Duration = Duration.S(value)
  def ms: Duration = Duration.Ms(value)
  def px: Length = Length.Px(value)
  def pt: Length = Length.Pt(value)
  def in: Length = Length.In(value)
  def pc: Length = Length.Pc(value)
  def cm: Length = Length.Cm(value)
  def mm: Length = Length.Mm(value)
  def em: Length = Length.Em(value)
  def ex: Length = Length.Ex(value)
  def ch: Length = Length.Ch(value)
  def rem: Length = Length.Rem(value)
  def vw: Length = Length.Vw(value)
  def vh: Length = Length.Vh(value)
  def vmin: Length = Length.Vmin(value)
  def vmax: Length = Length.Vmax(value)
