/*
    Cataract, version 0.1.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cataract

import rudiments.*
import language.dynamics

case class Stylesheet(rules: StylesheetItem*):
  override def toString(): String = rules.map(_.toString).join("\n")

trait StylesheetItem

case class Keyframes(name: String)(frames: Keyframe*) extends StylesheetItem:
  override def toString = frames.map(_.toString).join("@keyframes "+name+" {\n  ", "\n  ", "\n}\n")
  
case class Keyframe(ref: String, style: Style):
  override def toString = style.properties.map(_.toString). join(str"$ref { ", "; ", " }")

object From extends Dynamic:
  inline def applyDynamicNamed(method: "apply")(inline properties: (Label, Any)*): Keyframe =
    ${Macro.keyframe('{"from"}, 'properties)}

object To extends Dynamic:
  inline def applyDynamicNamed(method: "apply")(inline properties: (Label, Any)*): Keyframe =
    ${Macro.keyframe('{"to"}, 'properties)}
  
case class Import(url: String) extends StylesheetItem:
  override def toString(): String = str"@import url('$url');"

case class Style(properties: CssProperty*):
  override def toString(): String = properties.map(_.toString).join("\n")
  def apply(nested: (Selector => Rule)*): Selector => Stylesheet = sel =>
    Stylesheet(nested.map(_(sel))*)

case class Rule(selector: Selector, style: Style) extends StylesheetItem:
  override def toString(): String =
    val rules = style.properties.map(_.toString).join("; ")
    str"${selector.normalize.value} { $rules }"

case class CssProperty(key: String, value: String):
  override def toString(): String = str"$key: $value"

object Css extends Dynamic:
  inline def applyDynamicNamed(method: "apply")(inline properties: (Label, Any)*): Style =
    ${Macro.read('properties)}

// case class Selector(value: String) extends Dynamic:
//   inline def applyDynamicNamed(method: "apply")(inline properties: (Label, Any)*): Rule =
//     ${Macro.rule('this, 'properties)}
    
sealed trait Selector(val value: String):
  inline def applyDynamicNamed(method: "apply")(inline properties: (Label, Any)*): Rule =
    ${Macro.rule('this, 'properties)}
  
  def normalize: Selector
  
  def |(that: Selector): Selector = Selector.Or(this, that)
  def >>(that: Selector): Selector = Selector.Descendant(this, that)
  def >(that: Selector): Selector = Selector.Child(this, that)
  def +(that: Selector): Selector = Selector.After(this, that)
  def &(that: Selector): Selector = Selector.And(this, that)
  def ~(that: Selector): Selector = Selector.Before(this, that)

object Selector:
  case class Element(element: String) extends Selector(element):
    def normalize: Selector = this

  case class Before(left: Selector, right: Selector)
  extends Selector(str"${left.value}~${right.value}"):
    def normalize: Selector = left.normalize match
      case Or(a, b) => Or(Before(a, right).normalize, Before(b, right).normalize)
      case left     => right.normalize match
        case Or(a, b) => Or(Before(left, a).normalize, Before(left, b).normalize)
        case right    => Before(left, right)
      
  case class After(left: Selector, right: Selector)
  extends Selector(str"${left.value}+${right.value}"):
    def normalize: Selector = left.normalize match
      case Or(a, b) => Or(After(a, right).normalize, After(b, right).normalize)
      case left     => right.normalize match
        case Or(a, b) => Or(After(left, a).normalize, After(left, b).normalize)
        case right    => After(left, right)
      
  case class Id(id: String) extends Selector(str"#$id"):
    def normalize: Selector = this
  
  case class Class(cls: String) extends Selector(str".$cls"):
    def normalize: Selector = this
  
  case class PseudoClass(name: String) extends Selector(str":$name"):
    def normalize: Selector = this

  case class And(left: Selector, right: Selector)
  extends Selector(str"${left.value}${right.value}"):
    def normalize: Selector = left.normalize match
      case Or(a, b) => Or(And(a, right).normalize, And(b, right).normalize)
      case left     => right.normalize match
        case Or(a, b) => Or(And(left, a).normalize, And(left, b).normalize)
        case right    => And(left, right)

  case class Or(left: Selector, right: Selector)
  extends Selector(str"${left.value}, ${right.value}"):
    def normalize: Selector = Or(left.normalize, right.normalize)

  case class Descendant(left: Selector, right: Selector)
  extends Selector(str"${left.value} ${right.value}"):
    def normalize: Selector = left.normalize match
      case Or(a, b) => Or(Descendant(a, right).normalize, Descendant(b, right).normalize)
      case left     => right.normalize match
        case Or(a, b) => Or(Descendant(left, a).normalize, Descendant(left, b).normalize)
        case right    => Descendant(left, right)

  case class Child(left: Selector, right: Selector)
  extends Selector(str"${left.value}>${right.value}"):
    def normalize: Selector = left.normalize match
      case Or(a, b) => Or(Child(a, right).normalize, Child(b, right).normalize)
      case left     => right.normalize match
        case Or(a, b) => Or(Child(left, a).normalize, Child(left, b).normalize)
        case right    => Child(left, right)