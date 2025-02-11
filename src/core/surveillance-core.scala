/*
    Surveillance, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package surveillance

import anticipation.*
import contingency.*
import fulminate.*
import prepositional.*
import rudiments.*

extension [PathType: Abstractable across Paths into Text](path: PathType)
  def watch[ResultType](lambda: Watch => ResultType): ResultType raises WatchError =
    val watchSet = Watch(List(path))
    lambda(watchSet).also:
      watchSet.unregister()

extension [PathType: Abstractable across Paths into Text](paths: Iterable[PathType])
  def watch[ResultType](lambda: Watch => ResultType): ResultType raises WatchError =
    val watchSet = Watch(paths)

    lambda(watchSet).also:
      watchSet.unregister()

export WatchEvent.{NewFile, NewDirectory, Modify, Delete}

given Realm = realm"surveillance"
