/*
    Polyvinyl, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package polyvinyl

enum Datatype:
  case Varchar, Numeric

import Datatype.*

abstract class GeneralSchema(val types: Map[String, Datatype])
extends Schema[Datatype]:
  
  type Result[D <: Datatype] = D match
    case Varchar.type => String
    case Numeric.type => Int

object Address extends GeneralSchema(Map("houseNo" -> Numeric, "street" -> Varchar, "city" -> Varchar)):
  transparent inline def record(inline lambda: String => Any): Record = ${build('lambda)}

object Person extends Schema[Datatype]:
  def types: Map[String, Datatype] = Map(
    "name"    -> Varchar,
    "age"     -> Numeric
  )

  transparent inline def record(inline lambda: String => Any): Record = ${build('lambda)}

  type Result[D <: Datatype] = D match
    case Varchar.type => String
    case Numeric.type => Int