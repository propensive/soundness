/*
    Deviation, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package deviation

export rudiments.ErrorMessage
import rudiments.*

case class AggregateError[ErrorType <: Exception](errors: List[ErrorType])
extends Error(err"aggregation of errors: ${Text(errors.map(_.toString).mkString("\n", "\n", ""))}")

extension (ctx: StringContext)
  transparent inline def err[TupleType <: Matchable](value: TupleType = EmptyTuple): ErrorMessage[Tuple] =
    inline value match
      case value: Tuple =>
        ErrorMessage[value.type](ctx.parts.map(Text(_)), value)
      
      case other: TupleType =>
        ErrorMessage[TupleType *: EmptyTuple](ctx.parts.map(Text(_)), other *: EmptyTuple)
  
