/*
    Spectacular, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package spectacular

import anticipation.*
import rudiments.*
import vacuous.*
import wisteria.*

import scala.deriving.*

import language.experimental.captureChecking

object InspectableDerivation extends Derivable[Inspectable]:
  inline def join[DerivationType <: Product: ProductReflection]: DerivationType is Inspectable = value =>
    fields(value):
      [FieldType] => field =>
        val text = context.text(field)
        if tuple then text else s"$label:$text"

    . mkString(if tuple then "(" else s"$typeName(", " ╱ ", ")").tt

  inline def split[DerivationType: SumReflection]: DerivationType is Inspectable = value =>
    variant(value):
      [VariantType <: DerivationType] => variant =>
        context.let(_.give(variant.inspect)).or(variant.inspect)
