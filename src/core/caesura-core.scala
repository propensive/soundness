/*
    Caesura, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package caesura

import gossamer.*

package dsvFormats:
  given DsvFormat as csv = DsvFormat(false, ',', '"', '"')
  given DsvFormat as csvWithHeader = DsvFormat(true, ',', '"', '"')
  given DsvFormat as tsv = DsvFormat(false, '\t', '"', '"')
  given DsvFormat as tsvWithHeader = DsvFormat(true, '\t', '"', '"')
  given DsvFormat as ssv = DsvFormat(false, ' ', '"', '"')
  given DsvFormat as ssvWithHeader = DsvFormat(true, ' ', '"', '"')

package dsvRedesignations:
  given DsvRedesignation as unchanged = identity(_)
  given DsvRedesignation as lowerDotted = _.uncamel.map(_.lower).join(t" ")
  given DsvRedesignation as lowerSlashed = _.uncamel.map(_.lower).join(t" ")
  given DsvRedesignation as capitalizedWords = _.uncamel.map(_.capitalize).join(t" ")
  given DsvRedesignation as lowerWords = _.uncamel.map(_.lower).join(t" ")

extension [ValueType: DsvEncodable](value: ValueType) def dsv: Row = ValueType.encode(value)

extension [ValueType: DsvEncodable](value: Seq[ValueType])
  def dsv: Dsv = Dsv(value.to(LazyList).map(ValueType.encode(_)))
