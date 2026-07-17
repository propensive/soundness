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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package embarcadero

import anticipation.*
import fulminate.*
import gossamer.*

object OciError:
  enum Reason(val number: Int) extends Clarification:
    case MissingLayout extends Reason(1)
    case UnsupportedLayout(version: Text) extends Reason(2)
    case MissingIndex extends Reason(3)
    case MissingBlob(digest: Text) extends Reason(4)
    case DigestMismatch(expected: Text, actual: Text) extends Reason(5)
    case InvalidBlob(digest: Text, detail: Text) extends Reason(6)
    case UnsupportedDigest(algorithm: Text) extends Reason(7)
    case NoManifest extends Reason(8)
    case WriteUnsupported extends Reason(9)

  given communicable: Reason is Communicable =
    case Reason.MissingLayout =>
      m"the archive does not contain an oci-layout marker"

    case Reason.UnsupportedLayout(version) =>
      m"the image layout version $version is not supported"

    case Reason.MissingIndex =>
      m"the archive does not contain an index.json document"

    case Reason.MissingBlob(digest) =>
      m"the blob $digest is not present in the archive"

    case Reason.DigestMismatch(expected, actual) =>
      m"the blob addressed as $expected has the digest $actual"

    case Reason.InvalidBlob(digest, detail) =>
      m"the blob $digest could not be interpreted: $detail"

    case Reason.UnsupportedDigest(algorithm) =>
      m"the digest algorithm $algorithm is not supported"

    case Reason.NoManifest =>
      m"the image index lists no manifests"

    case Reason.WriteUnsupported =>
      m"OCI image archives cannot be opened for writing"

case class OciError(reason: OciError.Reason)(using Diagnostics)
extends Error(285, reason.number)(m"the OCI image archive could not be read because $reason")
