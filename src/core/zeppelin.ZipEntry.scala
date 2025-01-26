/*
    Zeppelin, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import prepositional.*
import proscenium.*
import serpentine.*
import turbulence.*

import java.nio.file as jnf

object ZipEntry:
  def apply[ResourceType: Readable by Bytes](path: Path on Zip, resource: ResourceType): ZipEntry =
    new ZipEntry(path, () => resource.stream[Bytes])

  given ZipEntry is Readable by Bytes = Readable.stream[Bytes].contramap(_.content())

  // 00:00:00, 1 January 2000
  val epoch: jnf.attribute.FileTime = jnf.attribute.FileTime.fromMillis(946684800000L).nn

case class ZipEntry(ref: Path on Zip, content: () => Stream[Bytes])
