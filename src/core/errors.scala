/*
    Digression, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package digression

import rudiments.*
import spectacular.*

import language.experimental.captureChecking

case class AggregateError[ErrorType <: Error](errors: List[ErrorType])
extends Error(err"aggregated errors:${Text(errors.map(_.message).mkString("\n", "\n", ""))}")

extension (ctx: StringContext)
  transparent inline def err(subs: Any*): ErrorMessage =
    def recur(subs: Seq[Any]): List[Text] =
      if subs.isEmpty then Nil else subs.head.debug :: recur(subs.tail)
    
    ErrorMessage(ctx.parts.to(List).map(Text(_)), recur(subs))
  
