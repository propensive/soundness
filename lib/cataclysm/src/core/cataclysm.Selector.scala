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
import proscenium.*

import language.dynamics

sealed trait Selector(val value: Text):
  inline def applyDynamicNamed(method: "apply")(inline properties: (Label, Any)*): CssRule =
    ${Cataclysm.rule('this, 'properties)}

  def normalize: Selector

  @targetName("or")
  infix def | (that: Selector): Selector = Selector.Or(this, that)

  @targetName("descendant")
  infix def >> (that: Selector): Selector = Selector.Descendant(this, that)

  @targetName("after")
  infix def + (that: Selector): Selector = Selector.After(this, that)

  @targetName("and")
  infix def & (that: Selector): Selector = Selector.And(this, that)

  @targetName("before")
  infix def ~ (that: Selector): Selector = Selector.Before(this, that)

object Selector:
  // given [SelectorType: Selectable, SelectorType2: Selectable]
  //     => CompareGreater[SelectorType, SelectorType2, Selector] as childSelector:

  //   inline def greaterThan(inline left: SelectorType, inline right: SelectorType2): Selector =
  //     Selector.Child(SelectorType.selector(left), SelectorType2.selector(right))

  case class Element(element: Text) extends Selector(element):
    def normalize: Selector = this

  case class Before(left: Selector, right: Selector)
  extends Selector(t"${left.value}~${right.value}"):
    def normalize: Selector = left.normalize match
      case Or(a, b) => Or(Before(a, right).normalize, Before(b, right).normalize)
      case left     => right.normalize match
        case Or(a, b) => Or(Before(left, a).normalize, Before(left, b).normalize)
        case right    => Before(left, right)

  case class After(left: Selector, right: Selector)
  extends Selector(t"${left.value}+${right.value}"):
    def normalize: Selector = left.normalize match
      case Or(a, b) => Or(After(a, right).normalize, After(b, right).normalize)
      case left     => right.normalize match
        case Or(a, b) => Or(After(left, a).normalize, After(left, b).normalize)
        case right    => After(left, right)

  case class Id(id: Text) extends Selector(t"#$id"):
    def normalize: Selector = this

  case class Class(cls: Text) extends Selector(t".$cls"):
    def normalize: Selector = this

  case class PseudoClass(name: Text) extends Selector(t":$name"):
    def normalize: Selector = this

  case class And(left: Selector, right: Selector)
  extends Selector(t"${left.value}${right.value}"):
    def normalize: Selector = left.normalize match
      case Or(a, b) => Or(And(a, right).normalize, And(b, right).normalize)
      case left     => right.normalize match
        case Or(a, b) => Or(And(left, a).normalize, And(left, b).normalize)
        case right    => And(left, right)

  case class Or(left: Selector, right: Selector)
  extends Selector(t"${left.value}, ${right.value}"):
    def normalize: Selector = Or(left.normalize, right.normalize)

  case class Descendant(left: Selector, right: Selector)
  extends Selector(t"${left.value} ${right.value}"):
    def normalize: Selector = left.normalize match
      case Or(a, b) => Or(Descendant(a, right).normalize, Descendant(b, right).normalize)
      case left     => right.normalize match
        case Or(a, b) => Or(Descendant(left, a).normalize, Descendant(left, b).normalize)
        case right    => Descendant(left, right)

  case class Child(left: Selector, right: Selector)
  extends Selector(t"${left.value}>${right.value}"):
    def normalize: Selector = left.normalize match
      case Or(a, b) => Or(Child(a, right).normalize, Child(b, right).normalize)
      case left     => right.normalize match
        case Or(a, b) => Or(Child(left, a).normalize, Child(left, b).normalize)
        case right    => Child(left, right)
