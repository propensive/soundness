                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package zeppelin

import anticipation.*
import fulminate.*
import prepositional.*
import serpentine.*

object ZipError:
  enum Reason(val number: Int) extends Clarification:
    case DuplicateEntry(path: Path on Zip)   extends Reason(1)
    case NotFound(path: Path on Zip)         extends Reason(2)
    case InvalidName(name: Text)             extends Reason(3)
    case UnsupportedMethod(method: Int)      extends Reason(4)
    case MissingEocd                         extends Reason(5)
    case TruncatedArchive                    extends Reason(6)
    case BadSignature(expected: Int)         extends Reason(7)
    case Zip64Error                          extends Reason(8)
    case WriteUnsupported                    extends Reason(9)
    case AlreadyExists                       extends Reason(10)
    case CannotWrite(detail: Text)           extends Reason(11)

  given communicable: Reason is Communicable =
    case Reason.DuplicateEntry(path)    => m"the path $path is a duplicate entry"
    case Reason.NotFound(path)          => m"path $path was not found in the ZIP file"
    case Reason.InvalidName(name)       => m"the name $name is not valid for a ZIP entry"
    case Reason.UnsupportedMethod(code) => m"the compression method $code is not supported"
    case Reason.MissingEocd             => m"no end-of-central-directory record could be found"
    case Reason.TruncatedArchive        => m"the ZIP archive ended unexpectedly"
    case Reason.BadSignature(expected)  => m"an expected record signature ($expected) was absent"
    case Reason.Zip64Error              => m"the ZIP64 metadata could not be interpreted"
    case Reason.WriteUnsupported        => m"ZIP archives cannot yet be opened for writing"
    case Reason.AlreadyExists           => m"an archive already exists at this path"
    case Reason.CannotWrite(detail)     => m"the archive could not be written: $detail"

case class ZipError(reason: ZipError.Reason)(using Diagnostics)
extends Error(751, reason.number)(m"the ZIP operation failed because $reason")
