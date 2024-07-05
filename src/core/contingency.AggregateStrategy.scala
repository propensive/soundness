/*
    Contingency, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package contingency

import language.experimental.pureFunctions

import java.util.concurrent.atomic as juca

import fulminate.*
import rudiments.*

@capability
class AggregateStrategy
    [ErrorType <: Exception, SuccessType]
    (label: boundary.Label[Either[AggregateError[ErrorType], SuccessType]])
extends Errant[ErrorType]:

  private val collected: juca.AtomicReference[List[ErrorType]] = juca.AtomicReference(Nil)
  def record(error: ErrorType): Unit = collected.getAndUpdate(error :: _.nn)

  def abort(error: ErrorType): Nothing =
    boundary.break(Left(AggregateError(error :: collected.get().nn)))(using label)

  def finish(): Unit =
    if !collected.get().nn.isEmpty
    then boundary.break(Left(AggregateError(collected.get().nn)))(using label)
