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
package embarcadero

import proscenium.compat.*

import anticipation.*
import aperture.*
import bitumen.*
import contingency.*
import prepositional.*
import turbulence.*
import zephyrine.*
import rudiments.*

// Opening a filesystem *path* as an OCI image, delegating the TAR bracket to bitumen's disk-backed
// `TarOpenable`. Split from `embarcadero.oci`'s cross-platform sources because it needs
// `bitumen.jvm`; the in-memory `data.open[Image]` (via `Image.DataOpenable`) stays in the core.
class ImageOpenable[path: Abstractable across Paths to Text]
  ( using Tactic[Oci.Error], Tactic[Tar.Error], Tactic[Truncation.Error] )
extends Openable:

  type Self = path
  type Form = Image
  type Operand = Nothing
  type Result = Image.Handle

  def open[grants <: Grant, result]
    ( value: path, mode: Mode granting grants, flags: List[Nothing] )
    ( block: ((Image.Handle & Granting[grants])^) ?=> result )
  :   result =

    if mode.atoms.has(Write) then abort(Oci.Error(Oci.Error.Reason.WriteUnsupported))

    TarOpenable[path]().open(value, mode, Nil): tar ?=>
      block(using new Image.Handle(tar.entries.to(List)) with Granting[grants] {})

// Re-exported through `soundness.*`, so `path.open[Image]` resolves on the JVM as before.
given imagePathOpenable: [path: Abstractable across Paths to Text]
=>  ( ociTactic: Tactic[Oci.Error], tarTactic: Tactic[Tar.Error], streamTactic: Tactic[Truncation.Error] )
=>  ( ImageOpenable[path]^{ociTactic, tarTactic, streamTactic} ) =
  ImageOpenable[path]
