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

import rudiments.*

import language.experimental.pureFunctions

enum Attempt[+SuccessType, +ErrorType <: Exception]:
  case Success(value: SuccessType)
  case Failure(value: ErrorType)

  def failure: Boolean = this match
    case Success(_) => false
    case Failure(_) => true

  def success: Boolean = !failure

  def map[SuccessType2](lambda: SuccessType => SuccessType2): Attempt[SuccessType2, ErrorType] =
    this match
      case Success(success) => Success(lambda(success))
      case Failure(failure) => Failure(failure)

  def handle(block: ErrorType ~> Exception): Attempt[SuccessType, Exception] =
    this match
      case Success(value) => Success(value)
      case Failure(value) => Failure(if block.isDefinedAt(value) then block(value) else value)

  def acknowledge(block: ErrorType ~> Unit): Attempt[SuccessType, ErrorType] =
    this match
      case Failure(value) => if block.isDefinedAt(value) then block(value)
      case _              => ()

    this

  transparent inline def apply(): SuccessType raises ErrorType = this match
    case Success(value) => value
    case Failure(error) => abort(error)

  def recover[SuccessType2 >: SuccessType](block: ErrorType ~> SuccessType2)
          : SuccessType2 =

    this match
      case Success(value) => value
      case Failure(error) => block(error)
