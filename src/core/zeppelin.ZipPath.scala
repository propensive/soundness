/*
    Zeppelin, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package zeppelin

import rudiments.*
import gossamer.*
import contingency.*
import serpentine.*
import anticipation.*
import turbulence.*
import prepositional.*

object ZipPath:
  given ZipPath is Navigable[InvalidZipNames, ZipFile]:
    def root(path: ZipPath): ZipFile = path.zipFile
    def descent(path: ZipPath): List[Name[InvalidZipNames]] = path.descent
    def prefix(path: ZipFile): Text = t"/"
    def separator(path: ZipPath): Text = t"/"

  given PathCreator[ZipPath, InvalidZipNames, ZipFile] as creator = (root, descent) =>
    ZipPath(root, ZipRef(descent))

  given (using Tactic[StreamError]) => ZipPath is Readable by Bytes as readable =
    Readable.lazyList[Bytes].contramap(_.entry().content())

case class ZipPath(zipFile: ZipFile, ref: ZipRef):
  def entry(): ZipEntry raises StreamError = zipFile.entry(ref)
