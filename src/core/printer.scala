/*
    Gossamer, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

// package gossamer

// trait Printer[ValueType]:
//   type WorkType
//   type ResultType
//   def append(current: WorkType, next: Text): WorkType
//   def result(value: WorkType): ResultType

// trait BufferPrinter[ValueType] extends Printer[ValueType]:
//   type WorkType = Unit
//   type ResultType = String
//   private val buffer: StringBuilder = StringBuilder()
//   def append(current: Unit, next: Text): Unit = buf.append(next.s)
//   def result(value: Unit): Unit