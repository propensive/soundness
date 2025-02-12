                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                    ╭───╮                                         ┃
┃  ╭─────────╮                                       │   │                                         ┃
┃  │   ╭─────╯╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮  ┃
┃  │   ╰─────╮│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯  ┃
┃  ╰─────╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ├╌╯╌─╯╰─╌ ╰───╮╰─╌ ╰───╮  ┃
┃  ╭─────╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╌   │╭───╌   │  ┃
┃  ╰─────────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯  ┃
┃                                                                                                  ┃
┃    Soundness, version 0.27.0. © Copyright 2023-25 Jon Pretty, Propensive OÜ.                     ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        http://www.apache.org/licenses/LICENSE-2.0                                                ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package caesura

import gossamer.*
import proscenium.*

package dsvFormats:
  given csv: DsvFormat = DsvFormat(false, ',', '"', '"')
  given csvWithHeader: DsvFormat = DsvFormat(true, ',', '"', '"')
  given tsv: DsvFormat = DsvFormat(false, '\t', '"', '"')
  given tsvWithHeader: DsvFormat = DsvFormat(true, '\t', '"', '"')
  given ssv: DsvFormat = DsvFormat(false, ' ', '"', '"')
  given ssvWithHeader: DsvFormat = DsvFormat(true, ' ', '"', '"')

package dsvRedesignations:
  given unchanged: DsvRedesignation = identity(_)
  given lowerDotted: DsvRedesignation = _.uncamel.map(_.lower).join(t" ")
  given lowerSlashed: DsvRedesignation = _.uncamel.map(_.lower).join(t" ")
  given capitalizedWords: DsvRedesignation = _.uncamel.map(_.capitalize).join(t" ")
  given lowerWords: DsvRedesignation = _.uncamel.map(_.lower).join(t" ")

extension [ValueType: DsvEncodable](value: ValueType) def dsv: Row = ValueType.encode(value)

extension [ValueType: DsvEncodable](value: Seq[ValueType])
  def dsv: Dsv = Dsv(value.to(Stream).map(ValueType.encode(_)))
