                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package phoenicia

import scala.compiletime.constValue

import anticipation.*
import gossamer.*
import prepositional.*
import spectacular.*
import symbolism.*

object Face:
  // Marks a part of a face's request that is decided at runtime — a computed weight, a feature
  // by its tag — and so is checked only when the font is made, not as the code compiles.
  sealed trait Runtime

  // A face with every part of its request in its type, as the fluent methods return it.
  type Shape[family, weights, slanting, enabled] =
    Face:
      type Topic = family
      type Weights = weights
      type Slanting = slanting
      type Enabled = enabled

  object Feature:
    // An OpenType layout feature is a four-character tag, which is also the feature's `Topic`:
    // `Feature.OldstyleNumerals` is a `Feature of "onum"`, so a face that enables it records the
    // tag in its type. The common registered features have names, stylistic sets and character
    // variants are numbered, and any other tag is admitted as it is, known only at runtime.
    def apply(tag: Text): Feature of Runtime = new Feature(tag) { type Topic = Runtime }

    private inline def feature[tag <: Label]: Feature of tag = tagged[tag](constValue[tag].tt)
    private def tagged[tag <: Label](tag: Text): Feature of tag =
      new Feature(tag) { type Topic = tag }

    val Ligatures: Feature of "liga" = feature["liga"]
    val DiscretionaryLigatures: Feature of "dlig" = feature["dlig"]
    val HistoricalLigatures: Feature of "hlig" = feature["hlig"]
    val ContextualAlternates: Feature of "calt" = feature["calt"]
    val Kerning: Feature of "kern" = feature["kern"]
    val OldstyleNumerals: Feature of "onum" = feature["onum"]
    val LiningNumerals: Feature of "lnum" = feature["lnum"]
    val TabularNumerals: Feature of "tnum" = feature["tnum"]
    val ProportionalNumerals: Feature of "pnum" = feature["pnum"]
    val SlashedZero: Feature of "zero" = feature["zero"]
    val Fractions: Feature of "frac" = feature["frac"]
    val Ordinals: Feature of "ordn" = feature["ordn"]
    val SmallCapitals: Feature of "smcp" = feature["smcp"]
    val CapitalSmallCapitals: Feature of "c2sc" = feature["c2sc"]
    val Swashes: Feature of "swsh" = feature["swsh"]
    val Superscript: Feature of "sups" = feature["sups"]
    val Subscript: Feature of "subs" = feature["subs"]

    def stylisticSet(number: Int): Feature of Runtime = Feature(t"ss${twoDigits(number)}")
    def characterVariant(number: Int): Feature of Runtime = Feature(t"cv${twoDigits(number)}")

    private def twoDigits(number: Int): Text =
      if number < 10 then t"0${number.show}" else number.show

    // A feature with the value it is set to: 0 disables it, 1 enables it, and some features
    // (character variants, for example) select among alternates by higher values.
    case class Setting(feature: Feature, value: Int)

    given showable: [feature <: Feature] => feature is Showable = _.tag

    given inspectable: [feature <: Feature] => feature is Inspectable =
      feature => t"Face.Feature(${feature.tag.inspect})"

  class Feature(val tag: Text) extends Topical:
    def enabled: Feature.Setting = Feature.Setting(this, 1)
    def disabled: Feature.Setting = Feature.Setting(this, 0)
    def set(value: Int): Feature.Setting = Feature.Setting(this, value)

    override def equals(that: Any): Boolean = that match
      case other: Feature => other.tag == tag
      case _              => false

    override def hashCode: Int = tag.hashCode
    override def toString: String = tag.s

  // A face of a typeface: regular and upright unless told otherwise. The typeface's family is
  // the face's `Topic`, so `Inter.bold` is a `Face of "Inter"`.
  def apply[family <: Label]
    ( typeface:   Typeface of family,
      weight:     Weight                     = Weight.Regular,
      slant:      Slant                      = Slant.Upright,
      stretch:    Stretch                    = Stretch.Normal,
      variations: List[Variation]            = Nil,
      features:   List[Face.Feature.Setting] = Nil )
  :   Face.Shape[family, Runtime, Runtime, Runtime] =

    new Face(typeface, weight, slant, stretch, variations, features):
      type Topic = family
      type Weights = Runtime
      type Slanting = Runtime
      type Enabled = Runtime

  given showable: [face <: Face] => face is Showable = face =>
    val slant = if face.slant.upright then t"" else t" ${face.slant.show}"
    t"${face.typeface.name} ${face.weight.show}$slant"

  given inspectable: [face <: Face] => face is Inspectable = face =>
    t"Face(${face.typeface.name.inspect}, ${face.weight.inspect}, ${face.slant.inspect})"

// A description of how a typeface is to be set: its weight, slant and stretch, positions on any
// further axes of a variable font, and the layout features to enable or disable. A face says
// nothing about where the font comes from; pairing it with a provision makes a `Font`.
//
// Three type members record what the code asked for, where it is known as it compiles — a weight
// as its literal (`700`), a slant as `"upright"` or `"italic"`, the enabled features as a union
// of tags (`"onum" | "tnum"`, `Nothing` for none) — or `Face.Runtime` where it is not. When a
// font is made from a provision whose file is on the classpath, the request is checked against
// the file at compile time.
class Face
  ( val typeface:   Typeface,
    val weight:     Weight,
    val slant:      Slant,
    val stretch:    Stretch,
    val variations: List[Variation],
    val features:   List[Face.Feature.Setting] )
extends Topical:

  type Topic <: Label
  type Weights
  type Slanting
  type Enabled

  private def retyped[weights, slanting, enabled]
    ( weight:     Weight                     = weight,
      slant:      Slant                      = slant,
      stretch:    Stretch                    = stretch,
      variations: List[Variation]            = variations,
      features:   List[Face.Feature.Setting] = features )
  :   Face.Shape[Topic, weights, slanting, enabled] =

    type family = Topic

    new Face(typeface, weight, slant, stretch, variations, features):
      type Topic = family
      type Weights = weights
      type Slanting = slanting
      type Enabled = enabled

  def weighing(weight: Weight): Face.Shape[Topic, Face.Runtime, Slanting, Enabled] =
    retyped(weight = weight)

  def thin: Face.Shape[Topic, 100, Slanting, Enabled] = retyped(weight = Weight.Thin)
  def light: Face.Shape[Topic, 300, Slanting, Enabled] = retyped(weight = Weight.Light)
  def regular: Face.Shape[Topic, 400, Slanting, Enabled] = retyped(weight = Weight.Regular)
  def medium: Face.Shape[Topic, 500, Slanting, Enabled] = retyped(weight = Weight.Medium)
  def semibold: Face.Shape[Topic, 600, Slanting, Enabled] = retyped(weight = Weight.Semibold)
  def bold: Face.Shape[Topic, 700, Slanting, Enabled] = retyped(weight = Weight.Bold)
  def black: Face.Shape[Topic, 900, Slanting, Enabled] = retyped(weight = Weight.Black)

  def slanted(slant: Slant): Face.Shape[Topic, Weights, Face.Runtime, Enabled] =
    retyped(slant = slant)

  def italic: Face.Shape[Topic, Weights, "italic", Enabled] = retyped(slant = Slant.Italic)
  def upright: Face.Shape[Topic, Weights, "upright", Enabled] = retyped(slant = Slant.Upright)

  def oblique(angle: Double): Face.Shape[Topic, Weights, "italic", Enabled] =
    retyped(slant = Slant.Oblique(angle))

  def stretched(stretch: Stretch): Face.Shape[Topic, Weights, Slanting, Enabled] =
    retyped(stretch = stretch)

  def condensed: Face.Shape[Topic, Weights, Slanting, Enabled] = stretched(Stretch.Condensed)
  def expanded: Face.Shape[Topic, Weights, Slanting, Enabled] = stretched(Stretch.Expanded)

  def varying(axis: Variation.Axis, value: Double): Face.Shape[Topic, Weights, Slanting, Enabled] =
    retyped(variations = variations + List(Variation(axis, value)))

  // A setting made at runtime may enable anything, so the enabled features are no longer all known.
  def featuring(setting: Face.Feature.Setting)
  :   Face.Shape[Topic, Weights, Slanting, Enabled | Face.Runtime] =

    retyped(features = features + List(setting))

  def enabling[tag](feature: Face.Feature of tag)
  :   Face.Shape[Topic, Weights, Slanting, tag | Enabled] =

    retyped(features = features + List(feature.enabled))

  // Disabling a feature the font lacks changes nothing, so it adds no requirement.
  def disabling(feature: Face.Feature): Face.Shape[Topic, Weights, Slanting, Enabled] =
    retyped(features = features + List(feature.disabled))
