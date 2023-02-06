/*
    Cellulose, version 0.4.0. Copyright 2022-23 Jon Pretty, Propensive OÜ.

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

import polyvinyl.*
import gossamer.*
import rudiments.*
import deviation.*

abstract class RecordType(schema: CodlSchema) extends Schema[Arity]:
  import Arity.*

  lazy val types: Map[String, Arity] = schema.subschemas.collect:
    case CodlSchema.Entry(key: Text, sch) => key.s -> sch.arity
  .to(Map)

  type Result[T <: Arity] = T match
    case One.type        => Text
    case Many.type       => List[Text]
    case Optional.type   => Maybe[Text]
    case AtLeastOne.type => List[Text]
    case Unique.type     => Maybe[Text]

  transparent inline def apply(indexed: Indexed): Record = record: key =>
    
    try
      val data = indexed.selectDynamic(key)
      data.head.schema.mm(_.arity).or(One) match
        case One        => data.head.children.head.key
        case Many       => data.flatMap(_.children.to(List).map(_.key).collect { case text: Text => text })
        case Optional   => data.head.children.headOption.map(_.key).getOrElse(Unset)
        case AtLeastOne => data.head.children.map(_.key)
        case Unique     => data.head.children.head.key
    catch case err: MissingValueError => Unset
