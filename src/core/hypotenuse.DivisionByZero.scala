/*
    Hypotenuse, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hypotenuse

import language.experimental.into

trait DivisionByZero:
  type Wrap[ResultType]
  inline def divideU64(left: U64, right: U64): Wrap[U64]
  inline def divideI64(left: I64, right: I64): Wrap[I64]
  inline def divideU32(left: U32, right: U32): Wrap[U32]
  inline def divideI32(left: I32, right: I32): Wrap[I32]
  inline def divideU16(left: U16, right: U16): Wrap[U16]
  inline def divideI16(left: I16, right: I16): Wrap[I16]
  inline def divideU8(left: U8, right: U8): Wrap[U8]
  inline def divideI8(left: I8, right: I8): Wrap[I8]
