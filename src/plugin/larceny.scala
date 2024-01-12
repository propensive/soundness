/*
    Larceny, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package larceny

import dotty.tools.dotc.reporting as dtdr

def demilitarize(code: Matchable): List[CompileError] = code match
  case xs: List[CompileError] @unchecked => xs
  case _ => Nil

def deferCompilation(errors: List[CompileError]): List[CompileError] = errors
  
case class CompileError(id: Int, message: String, code: String, start: Int, offset: Int):
  def errorId: dtdr.ErrorMessageID = dtdr.ErrorMessageID.fromOrdinal(id)
  def point: Int = start + offset

object ErrorId:
  export dtdr.ErrorMessageID.*
  def unapply(compileError: CompileError): Some[dtdr.ErrorMessageID] = Some(compileError.errorId)
