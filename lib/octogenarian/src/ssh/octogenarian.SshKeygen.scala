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
package octogenarian

import ambience.*
import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import galilei.*
import gossamer.*
import guillotine.*
import prepositional.*
import rudiments.*
import serpentine.*
import symbolism.*
import turbulence.*
import vacuous.*
import zephyrine.*

// Signing and verifying with OpenSSH keys through `ssh-keygen -Y`, as `gpg.format = ssh` has git
// do: a signature is made over the payload in the `git` namespace with a private key, and
// verified against an allowed-signers file, which names the principals each public key speaks
// for.
object SshKeygen:
  import Git.Error.Reason as Reason

  private val namespace: Text = t"git"

  // Runs `ssh-keygen` with `payload` on its standard input, returning its output and exit status.
  private def run(arguments: List[Text], payload: Data)
    ( using working:     WorkingDirectory,
            environment: Environment,
            loggable:    (Exec.Event is Loggable)^,
            tactic:      Tactic[Git.Error] )
  :   (Text, Int) =

    given execution: Tactic[Exec.Error] = tactic.contramap: error =>
      Git.Error(Reason.SigningFailed(error.message.text))(using Diagnostics.omit)

    given truncation: Tactic[Truncation.Error] = tactic.contramap: error =>
      Git.Error(Reason.SigningFailed(error.message.text))(using Diagnostics.omit)

    val job = Command((t"ssh-keygen" :: arguments)*).fork[Text]()
    job.stdin(payload.stream)
    val output = job.await()
    (output, job.status())

  private def temporary(content: Text): Path on Linux =
    val file = java.nio.file.Files.createTempFile("octogenarian-", ".sig").nn
    java.nio.file.Files.writeString(file, content.s)
    unsafely(file.toString.tt.as[Path on Linux])

  // Signs with the private key in `key`.
  class Signer(key: Path on Linux)
    ( using working:     WorkingDirectory,
            environment: Environment,
            loggable:    (Exec.Event is Loggable)^ )
  extends Git.Signer:

    def sign(payload: Data)(using Tactic[Git.Error]): Text =
      val (output, status) = run(List(t"-Y", t"sign", t"-n", namespace, t"-f", key.encode), payload)
      if status != 0 then abort(Git.Error(Reason.SigningFailed(output.trim)))
      output

  // Verifies against the principals and keys in `allowedSigners`.
  class Verifier(allowedSigners: Path on Linux)
    ( using working:     WorkingDirectory,
            environment: Environment,
            loggable:    (Exec.Event is Loggable)^ )
  extends Git.Verifier:

    def verify(payload: Data, signature: Text)(using Tactic[Git.Error]): Git.Verification =
      val file = temporary(signature)

      try
        val (principals, found) =
          run
            ( List(t"-Y", t"find-principals", t"-s", file.encode, t"-f", allowedSigners.encode),
              payload )

        if found != 0 then Git.Verification.Unknown(principals.trim) else
          val principal = principals.cut(t"\n").prim.or(t"").trim

          val arguments =
            List
              ( t"-Y", t"verify", t"-n", namespace, t"-f", allowedSigners.encode, t"-I", principal,
                t"-s", file.encode )

          val (output, status) = run(arguments, payload)

          if status == 0 then Git.Verification.Good(principal)
          else Git.Verification.Bad(output.trim)
      finally java.nio.file.Files.deleteIfExists(java.nio.file.Path.of(file.encode.s))
