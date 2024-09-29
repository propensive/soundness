/*
    Galilei, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package galilei

import anticipation.*
import contextual.*
import contingency.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import turbulence.*

import scala.compiletime.*
import scala.jdk.StreamConverters.*

import java.io as ji
import java.nio as jn
import java.nio.file as jnf

import language.experimental.pureFunctions

object File:
  given File is Inspectable as inspectable = file => t"file:${file.path.render}"

  given [FileType <: File](using Tactic[StreamError], Tactic[IoError])
      => FileType is Readable by Bytes as readableBytes =
    Readable.inputStream.contramap: file =>
      try ji.BufferedInputStream(jnf.Files.newInputStream(file.path.stdlib))
      catch case _: jnf.NoSuchFileException => abort(IoError(file.path))

  given (using io: Tactic[IoError], streamCut: Tactic[StreamError])
      => File is Writable by Bytes as writableBytes =

    Writable.outputStreamBytes.contramap: file =>
      if !file.writable() then abort(IoError(file.path))
      ji.BufferedOutputStream(ji.FileOutputStream(file.path.stdlib.toFile, false))

  given (using io: Tactic[IoError], streamCut: Tactic[StreamError])
      => File is Appendable by Bytes as appendableBytes =
    Appendable.outputStreamBytes.contramap: file =>
      if !file.writable() then abort(IoError(file.path))
      ji.BufferedOutputStream(ji.FileOutputStream(file.path.stdlib.toFile, true))

  given File is GenericFile = _.path.fullname
