/*
    Acyclicity, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package acyclicity

import language.experimental.captureChecking

import anticipation.*
import contextual.*
import fulminate.*
import gossamer.*
import spectacular.*

object NodeParser extends Interpolator[Unit, Option[Dot.Ref], Dot.Ref]:
  private val compassPoints: Set[Text] = Set(t"n", t"e", t"s", t"w", t"ne", t"nw", t"se", t"sw")
  def parse(state: Option[Dot.Ref], next: Text): Some[Dot.Ref] =
    Some { next.show.cut(t":").to(List) match
      case List(id) =>
        Dot.Ref(Dot.Id(id))

      case List(id, port) =>
        Dot.Ref(Dot.Id(id), Some(Dot.Attachment(Dot.Id(port.show))))

      case List(id, port, point) if compassPoints.contains(point) =>
        Dot.Ref(Dot.Id(id), Some(Dot.Attachment(Dot.Id(port),
            Some(Dot.CompassPoint.valueOf(point.capitalize.s)))))

      case _ =>
        import errorDiagnostics.empty
        throw InterpolationError(m"not a valid node ID")
    }

  def initial: Option[Dot.Ref] = None
  def complete(value: Option[Dot.Ref]): Dot.Ref = value.get
  def skip(state: Option[Dot.Ref]): Option[Dot.Ref] = state
  def insert(state: Option[Dot.Ref], value: Unit): Option[Dot.Ref] = state
