/*
    Cataclysm, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import gossamer.*
import vacuous.*
import anticipation.*
import hieroglyph.*

import annotation.targetName
import language.dynamics

object CssStylesheet:
  given (using charEncoder: CharEncoder)
      : GenericHttpResponseStream[CssStylesheet] with
    def mediaType: Text = t"text/css; charset=${charEncoder.encoding.name}"
    def content(stylesheet: CssStylesheet): LazyList[IArray[Byte]] = LazyList(stylesheet.text.bytes)
  
  trait Item:
    def text: Text

case class CssStylesheet(rules: CssStylesheet.Item*):
  def text: Text = rules.map(_.text).join(t"\n")

case class MediaRule(query: Text)(rules: CssStylesheet.Item*) extends CssStylesheet.Item:
  def text: Text = rules.map(t"  "+_.text).join(t"@media $query {\n", t"\n", t"\n}")

case class Keyframes(name: Text)(frames: Keyframe*) extends CssStylesheet.Item:
  def text: Text = frames.map(_.text).join(t"@keyframes ${name} {\n  ", t"\n  ", t"\n}\n")
  
case class Keyframe(ref: Text, style: CssStyle):
  def text: Text = style.properties.map(_.text). join(t"$ref { ", t"; ", t" }")

object From extends Dynamic:
  inline def applyDynamicNamed(method: "apply")(inline properties: (Label, Any)*): Keyframe =
    ${Cataclysm.keyframe('{"from"}, 'properties)}

object To extends Dynamic:
  inline def applyDynamicNamed(method: "apply")(inline properties: (Label, Any)*): Keyframe =
    ${Cataclysm.keyframe('{"to"}, 'properties)}

case class FontFace
    (ascentOverride: Optional[Text] = Unset, descentOverride: Optional[Text] = Unset,
        fontDisplay: Optional[Text] = Unset, fontFamily: Optional[Text] = Unset,
        fontStretch: Optional[Text] = Unset, fontStyle: Optional[Text] = Unset,
        fontWeight: Optional[Text] = Unset, fontVariationSettings: Optional[Text] = Unset,
        lineGapOverride: Optional[Text] = Unset, sizeAdjust: Optional[Text] = Unset,
        src: Optional[Text] = Unset, unicodeRange: Optional[Text] = Unset)
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
    .join(t"@font-face { ", t" ", t" }")
    

case class Import(url: Text) extends CssStylesheet.Item:
  def text: Text = t"@import url('$url');"

object CssStyle:
  given GenericHtmlAttribute["style", CssStyle] with
    def serialize(value: CssStyle): Text = value.properties.map(_.text).join(t";")
    def name: Text = t"style"

case class CssStyle(properties: CssProperty*):
  def text: Text = properties.map(_.text).join(t"\n")
  
  def apply(nested: (Selector => CssRule)*): Selector => CssStylesheet = sel =>
    CssStylesheet(nested.map(_(sel))*)

case class CssRule(selector: Selector, style: CssStyle) extends CssStylesheet.Item:
  def text: Text =
    val rules = style.properties.map(_.text).join(t"; ")
    t"${selector.normalize.value} { $rules }"

case class CssProperty(key: Text, value: Text):
  def text: Text = t"$key: $value"

object Css extends Dynamic:
  def applyDynamic(method: "apply")(): CssStyle = CssStyle()

  inline def applyDynamicNamed(method: "apply")(inline properties: (Label, Any)*): CssStyle =
    ${Cataclysm.read('properties)}

sealed trait Selector(val value: Text):
  inline def applyDynamicNamed(method: "apply")(inline properties: (Label, Any)*): CssRule =
    ${Cataclysm.rule('this, 'properties)}
  
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
