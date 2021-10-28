/*
    Cataract, version 0.6.0. Copyright 2021-21 Jon Pretty, Propensive OÜ.

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
import gossamer.*

import annotation.targetName
import language.dynamics

object Stylesheet:
  given clairvoyant.HttpResponse[Stylesheet, Txt] with
    def mimeType: String = "text/css; charset=utf-8"
    def content(stylesheet: Stylesheet): Txt = stylesheet.text
  
  trait Item:
    def text: Txt

case class Stylesheet(rules: Stylesheet.Item*):
  def text: Txt = rules.map(_.text).join(str"\n")

case class Keyframes(name: Txt)(frames: Keyframe*) extends Stylesheet.Item:
  def text: Txt = frames.map(_.text).join(str"@keyframes ${name} {\n  ", str"\n  ", str"\n}\n")
  
case class Keyframe(ref: Txt, style: Style):
  def text: Txt = style.properties.map(_.text). join(str"$ref { ", str"; ", str" }")

object From extends Dynamic:
  inline def applyDynamicNamed(method: "apply")(inline properties: (Label, Any)*): Keyframe =
    ${Macro.keyframe('{"from"}, 'properties)}

object To extends Dynamic:
  inline def applyDynamicNamed(method: "apply")(inline properties: (Label, Any)*): Keyframe =
    ${Macro.keyframe('{"to"}, 'properties)}
  
case class Import(url: Txt) extends Stylesheet.Item:
  def text: Txt = str"@import url('$url');"

object Style:
  given clairvoyant.HtmlAttribute["style", Style] with
    def serialize(value: Style): String = value.properties.map(_.text).join(str";").s
    def name: String = "style"

case class Style(properties: CssProperty*):
  def text: Txt = properties.map(_.text).join(str"\n")
  
  def apply(nested: (Selector => Rule)*): Selector => Stylesheet = sel =>
    Stylesheet(nested.map(_(sel))*)

case class Rule(selector: Selector, style: Style) extends Stylesheet.Item:
  def text: Txt =
    val rules = style.properties.map(_.text).join(str"; ")
    str"${selector.normalize.value} { $rules }"

case class CssProperty(key: Txt, value: Txt):
  def text: Txt = str"$key: $value"

object Css extends Dynamic:
  inline def applyDynamicNamed(method: "apply")(inline properties: (Label, Any)*): Style =
    ${Macro.read('properties)}

sealed trait Selector(val value: Txt):
  inline def applyDynamicNamed(method: "apply")(inline properties: (Label, Any)*): Rule =
    ${Macro.rule('this, 'properties)}
  
  def normalize: Selector

  @targetName("or")
  infix def |(that: Selector): Selector = Selector.Or(this, that)
  
  @targetName("descendant")
  infix def >>(that: Selector): Selector = Selector.Descendant(this, that)
  
  @targetName("child")
  infix def >(that: Selector): Selector = Selector.Child(this, that)
  
  @targetName("after")
  infix def +(that: Selector): Selector = Selector.After(this, that)
  
  @targetName("and")
  infix def &(that: Selector): Selector = Selector.And(this, that)
  
  @targetName("before")
  infix def ~(that: Selector): Selector = Selector.Before(this, that)

object Selector:
  case class Element(element: Txt) extends Selector(element):
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
      
  case class Id(id: Txt) extends Selector(str"#$id"):
    def normalize: Selector = this
  
  case class Class(cls: Txt) extends Selector(str".$cls"):
    def normalize: Selector = this
  
  case class PseudoClass(name: Txt) extends Selector(str":$name"):
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
