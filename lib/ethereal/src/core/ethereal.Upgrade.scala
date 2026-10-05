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
package ethereal

import ambience.*, systems.javaBaseSystem
import anticipation.*
import aperture.*
import contingency.*
import distillate.*
import fulminate.*
import galilei.*
import gossamer.*
import hieroglyph.*, charsets.utf8Charset
import textSanitizers.strictSanitizer
import nomenclature.*
import prepositional.*
import serpentine.*
import turbulence.*
import vacuous.*

import filesystemBackends.javaBaseFilesystem
import filesystemOptions.createNonexistentParents
import filesystemOptions.deleteOnlyEmpty
import filesystemOptions.dereferenceSymlinks
import filesystemOptions.moveAtomically
import filesystemOptions.overwritePreexisting

// Self-upgrade, from the daemon's side. An application stages the bytes of a complete signed
// executable as `<data>/<name>/.pending`, and the XEK launcher does the rest at the start of
// its next invocation: it verifies the candidate against the keys and application id in the
// *running* executable's record, swaps it into place, re-execs, and records what it did in
// `<data>/<name>/.upgrade-result`. Its staleness check then displaces this daemon by itself,
// so nothing here needs to exit or respawn anything: the clients this daemon is serving carry
// on undisturbed. The layout is xek's `spec/layout.md`, and the verification rule its
// `spec/ethrcfg.md`; the Scala side verifies nothing.
object Upgrade:
  // Staged by writing a temporary file beside `.pending` and renaming it into place, so a
  // launcher starting concurrently never reads a partial candidate.
  inline def stage[source]
    ( source: source )
    ( using environment: Environment,
            system:      System,
            diagnostics: Diagnostics,
            readable:    source is Readable to Data )
  :   Unit raises Upgrade.Error =

    stageBytes(source.read[Data])


  private def stageBytes(bytes: Data)
    ( using environment: Environment, system: System, diagnostics: Diagnostics )
  :   Unit raises Upgrade.Error =

    mitigate:
      case Path.Error(_, _)     => Upgrade.Error(Upgrade.Error.Reason.CannotResolveName)
      case Property.Error(_)    => Upgrade.Error(Upgrade.Error.Reason.CannotResolveName)
      case Io.Error(_, _, _, _) => Upgrade.Error(Upgrade.Error.Reason.CannotWritePending)
      case Name.Error(_, _, _)  => Upgrade.Error(Upgrade.Error.Reason.CannotWritePending)
      case Truncation.Error(_)  => Upgrade.Error(Upgrade.Error.Reason.CannotReadSource)

    . protect:
        val target: Path on Linux = directory()
        if !target.existent() then target.create[Directory](CreateFlag.Parents)
        val partial: Path on Linux = target/t".pending.tmp"

        partial.open[File](Write, OpenFlag.Create, OpenFlag.Truncate): file ?=>
          file.write(Chain(bytes))

        partial.moveTo(target/t".pending")


  // Whether a staged upgrade is waiting for the launcher, which applies it the next time the
  // command is run.
  def pending(using Environment, System): Boolean =
    safely((directory()/t".pending").existent()).or(false)

  // What the launcher last did with a staged upgrade, until it is acknowledged. A missing or
  // unreadable file, and an outcome this version does not know, are all `Unset`.
  def outcome(using Environment, System): Optional[Upgrade.Outcome] =
    safely:
      val result: Path on Linux = directory()/t".upgrade-result"

      if !result.existent() then Unset else result.read[Text].trim.cut(t" ") match
        case List(word, candidate, previous, time) =>
          Outcome.Result.parse(word).let: result =>
            Outcome(result, candidate.as[Long], previous.as[Long], time.as[Long])

        case _ =>
          Unset

    . or(Unset)

  // Forgets the last outcome, so that an application reports each one once.
  def acknowledge()(using Environment, System): Unit =
    safely((directory()/t".upgrade-result").wipe())

  // Whether the running executable's launcher would accept a signed upgrade at all: false for
  // one built without a release key and application id, such as a development build, and
  // false when the property is absent (an older launcher, or no launcher).
  def enabled(using System): Boolean =
    safely(System.properties.ethereal.upgradable[Text]()).let(_ == t"true").or(false)

  // The data directory the launcher reads (xek `spec/layout.md`): `$XDG_DATA_HOME`, or
  // `~/.local/share`, on Unix; `%LOCALAPPDATA%` on Windows, which is what
  // `Directories.cacheHome` resolves to there (`Directories.dataHome` would be the roaming
  // `%APPDATA%`, which the launcher does not look at).
  private def directory()(using Environment, System)
  :   Path on Linux raises Path.Error raises Property.Error raises Name.Error =

    val name: Text = System.properties.ethereal.name[Text]()

    val root: Path on Linux =
      if windows then Directories.cacheHome[Path on Linux] else Xdg.dataHome[Path on Linux]

    root/name


  private def windows(using System): Boolean =
    safely(System.properties.os.name[Text]().lower.starts(t"windows")).or(false)

  object Outcome:
    object Result:
      given communicable: Result is Communicable =
        case Applied          => m"the upgrade was applied"
        case Disabled         => m"the running executable does not accept upgrades"
        case NoRecord         => m"the candidate is not an XEK executable"
        case WrongApplication => m"the candidate was built for another application"
        case BadSignature     => m"the candidate's signature does not verify"
        case NotNewer         => m"the candidate is not newer than the running executable"
        case SwapFailed       => m"the candidate could not be swapped into place"

      // The word the launcher writes for each outcome.
      def parse(word: Text): Optional[Result] = word match
        case t"applied"           => Applied
        case t"disabled"          => Disabled
        case t"no-record"         => NoRecord
        case t"wrong-application" => WrongApplication
        case t"bad-signature"     => BadSignature
        case t"not-newer"         => NotNewer
        case t"swap-failed"       => SwapFailed
        case _                    => Unset

    enum Result:
      case Applied, Disabled, NoRecord, WrongApplication, BadSignature, NotNewer, SwapFailed

  // What the launcher did with a staged upgrade: `candidate` is the staged executable's build
  // id (zero if it had no record), and `previous` that of the executable which checked it.
  case class Outcome(result: Outcome.Result, candidate: Long, previous: Long, time: Long):
    def when[instant: Instantiable across Instants from Long]: instant = instant(time)

  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case CannotReadSource   => m"the upgrade source could not be read"
        case CannotWritePending => m"the .pending file could not be written"
        case CannotResolveName  => m"the running application's name is not available"

    enum Reason(val number: Int) extends Clarification:
      case CannotReadSource   extends Reason(1)
      case CannotWritePending extends Reason(2)
      case CannotResolveName  extends Reason(3)

  case class Error(reason: Upgrade.Error.Reason)(using Diagnostics)
  extends fulminate.Error(631, reason.number)(m"could not stage the upgrade because $reason")
