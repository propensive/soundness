/*
    Villainy, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package villainy

import anticipation.*
import contingency.*
import fulminate.*
import inimitable.*
import jacinta.*
import kaleidoscope.*
import merino.*
import urticose.*
import polyvinyl.*
import rudiments.*
import vacuous.*

import scala.compiletime.*

import strategies.throwUnsafely

case class JsonSchemaDoc
  ( `$schema`:  Text,
    `$id`:      Text,
    title:      Text,
    `type`:     Text,
    properties: Map[Text, JsonSchema.Property],
    required:   Optional[Set[Text]] ):

  lazy val requiredFields: Set[Text] = required.or(Set())

  def fields: Map[Text, Member] =
    properties.map { (key, value) => key -> value.field(requiredFields.contains(key)) }
