/*
    Wisteria, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package wisteria

import anticipation.*
import fulminate.*

import scala.deriving.*
import scala.compiletime.*

object VariantError:
  inline def apply[DerivationType](inputLabel: Text)
     (using reflection: SumReflection[DerivationType], diagnostics: Diagnostics)
  :     VariantError =

    val variants = constValueTuple[reflection.MirroredElemLabels].toList.map(_.toString.tt)
    val sum = constValue[reflection.MirroredLabel].tt

    VariantError(inputLabel, sum, variants)

case class VariantError(inputLabel: Text, sum: Text, validVariants: List[Text])(using Diagnostics)
extends Error
   (m"""the specified variant ($inputLabel) is not one of the valid variants
        (${validVariants.mkString(", ").tt}) of sum type $sum""")
