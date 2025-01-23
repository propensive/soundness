/*
    Panopticon, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package panopticon

import rudiments.*

import scala.quoted.*

export Panopticon.Lens

extension [FromType, PathType <: Tuple, ToType](lens: Lens[FromType, PathType, ToType])
  @targetName("append")
  infix def ++ [ToType2, PathType2 <: Tuple](right: Lens[ToType, PathType2, ToType2])
  :     Lens[FromType, Tuple.Concat[PathType, PathType2], ToType2] =

    Lens.make()

  inline def get(target: FromType): ToType = ${Panopticon.get[FromType, PathType, ToType]('target)}

  inline def set(target: FromType, newValue: ToType): FromType =
    ${Panopticon.set[FromType, PathType, ToType]('target, 'newValue)}
