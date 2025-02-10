/*
    Cellulose, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cellulose

import anticipation.*
import contingency.*
import rudiments.*
import vacuous.*

class CodlFieldReader[ValueType](lambda: Text => ValueType)
extends CodlDecoder[ValueType]:
  val schema: CodlSchema = Field(Arity.One)

  def decoded(nodes: List[Indexed]): ValueType raises CodlReadError =
    nodes.prim.lest(CodlReadError()).children match
      case IArray(CodlNode(Data(value, _, _, _), _)) => lambda(value)
      case _                                         => abort(CodlReadError())
