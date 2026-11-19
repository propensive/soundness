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
import aperture.*
import contingency.*
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

// Signing and verifying with OpenPGP keys through `gpg`, as git does: a detached, armoured
// signature over the payload, and verification read from the machine-readable status lines.
object Gpg:
  import Git.Error.Reason as Reason

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

    val job = Command((t"gpg" :: arguments)*).fork[Text]()
    job.stdin(payload.stream)
    val output = job.await()
    (output, job.status())

  // Writes `content` to a scratch file which only its owner may read, and lends its path to
  // `block`, deleting the file when `block` ends, however it ends. It lives in the JVM's own
  // temporary directory, `java.io.tmpdir`.
  private def temporary[result](content: Text)(block: Path on Linux => result)
    ( using tactic: Tactic[Git.Error] )
  :   result =

    import filesystemBackends.javaBaseFilesystem
    import hieroglyph.codepages.utf8Codepage
    import temporaryDirectories.javaBaseTemporaryDirectory

    given io: Tactic[Io.Error] = tactic.contramap: error =>
      Git.Error(Reason.SigningFailed(error.message.text))(using Diagnostics.omit)

    given truncation: Tactic[Truncation.Error] = tactic.contramap: error =>
      Git.Error(Reason.SigningFailed(error.message.text))(using Diagnostics.omit)

    given umask: Umask = Umask(0x3f) // 077: a file of mode 0600, which only its owner may read

    temporaryDirectory[Path on Linux].open[Scratch.File](Read & Write): file ?=>
      file.write(Chain(content.in[Data]))
      block(file.path)

  // Signs with the default key, or with `key` (a key id, fingerprint or user id).
  class Signer(key: Optional[Text] = Unset)
    ( using working:     WorkingDirectory,
            environment: Environment,
            loggable:    (Exec.Event is Loggable)^ )
  extends Git.Signer:

    def sign(payload: Data)(using Tactic[Git.Error]): Text =
      val keyArguments = key.let { key => List(t"-u", key) }.or(Nil)
      val arguments = List(t"--batch", t"--status-fd=2", t"-bsa") + keyArguments
      val (output, status) = run(arguments, payload)
      if status != 0 then abort(Git.Error(Reason.SigningFailed(output.trim)))
      output

  // Verifies against the keyring, reading `GOODSIG`, `BADSIG` and the rest from the status lines.
  class Verifier()
    ( using working:     WorkingDirectory,
            environment: Environment,
            loggable:    (Exec.Event is Loggable)^ )
  extends Git.Verifier:

    def verify(payload: Data, signature: Text)(using Tactic[Git.Error]): Git.Verification =
      temporary(signature): file =>
        val (output, status) =
          run(List(t"--batch", t"--status-fd=1", t"--verify", file.encode, t"-"), payload)

        val lines = output.cut(t"\n").map(_.trim).filter(_.starts(t"[GNUPG:] ")).map(_.skip(9))

        // `GOODSIG <key id> <user id>`: the user id is everything after the second space.
        def userId(line: Text): Text =
          val second = line.s.indexOf(' ', line.s.indexOf(' ') + 1)
          if second < 0 then line else line.skip(second + 1)

        lines.filter(_.starts(t"GOODSIG ")).head match
          case line: Text => Git.Verification.Good(userId(line))

          case _ =>
            lines.filter(_.starts(t"BADSIG ")).head match
              case line: Text => Git.Verification.Bad(line)
              case _          => Git.Verification.Unknown(lines.head.or(output.trim))
