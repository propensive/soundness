/*
    Tarantula, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package tarantula

import anticipation.*, durationApi.javaLong
import cataclysm.*
import contingency.*
import eucalyptus.*
import fulminate.*
import gastronomy.*
import gesticulate.*
import gossamer.*
import guillotine.*
import hallucination.*
import hieroglyph.*, charEncoders.utf8
import honeycomb.*
import jacinta.*, jsonPrinters.minimal, dynamicJsonAccess.enabled
import monotonous.*
import nettlesome.*
import parasite.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*

import strategies.throwUnsafely

import unsafeExceptions.canThrowAny

trait Focusable:
  type Self
  def strategy: Text
  def focus(value: Self): Text

object Focusable:
  def apply[ElementType](strategy0: Text, focus0: ElementType => Text): ElementType is Focusable =
    new Focusable:
      type Self = ElementType
      def strategy: Text = strategy0
      def focus(value: Self): Text = focus0(value)

  given Text is Focusable = Focusable(t"link text", identity(_))
  given Selector is Focusable = Focusable(t"css selector", _.normalize.value)
  given TagType[?, ?, ?] is Focusable = Focusable(t"tag name", _.label)
  given DomId is Focusable = Focusable(t"css selector", v => t"#${v.name}")
  given CssClass is Focusable = Focusable(t"css selector", v => t".${v.name}")
