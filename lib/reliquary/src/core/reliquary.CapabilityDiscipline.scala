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
package reliquary

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import prepositional.*
import rudiments.*
import stratiform.*
import turbulence.*
import vacuous.*

// The `capability/1` discipline (hosts.md §5): the discipline of host contracts with no formal
// carrier. It claims the single tree item at the path `capabilities` — a TEL document under the
// `lira-capabilities` schema — and emits one rigid atom per capability row: the key is the
// capability's name, and the canonical encoding is the UTF-8 bytes of the name, a 0x00
// separator, then 0x01 and the UTF-8 bytes of the version predicate where one is declared, or a
// single 0x00 where none is. A predicate therefore folds into the atom's value, so tightening
// or loosening one is a removal plus an addition — major, which is conservative and sound. The
// advisory `probe` field enters no atom: it participates in implementation identity (it is
// bytes in the payload) but never in API identity, so editing a probe is a patch.
//
// Like `opaque/1` and `resource/1` it is language-blind and lives in the core, since every
// verifier must be able to implement it (§16, step 4).
object CapabilityDiscipline extends Discipline:
  def id: Text = t"capability/1"

  def claims(path: TreePath, data: Data): Boolean = path.text == t"capabilities"

  // The single realm `{host}`: capability listings describe environments, never libraries, and
  // L127 rejects a library release that declares this discipline.
  def domain: Discipline.Domain = Discipline.Domain.Realms(Set(t"host"))
  def keying: Discipline.Keying = Discipline.Keying.Declaration

  // Presence, on the same terms as `resource/1` — the recompilation level for content addressed
  // by name, and the only level "the command exists" can mean.
  def guarantees(realm: Text): Set[Discipline.Guarantee] =
    Set(Discipline.Guarantee.Recompilation)

  private def malformed(detail: Text): Discipline.Error =
    import errorDiagnostics.emptyDiagnostics
    Discipline.Error(t"capability/1", Discipline.Error.Reason.Malformed(detail))

  def atomize(content: List[(TreePath, Data)], context: Discipline.Context)
  :   Atomization raises Discipline.Error =

    val atoms = content.stdlib.flatMap: (path, data) => rows(data)
    Atomization.of(id, List.from(atoms))

  private def rows(data: Data): scala.List[Atom] raises Discipline.Error =
    val document =
      import errorDiagnostics.emptyDiagnostics

      mitigate:
        case Tel.Error(reason, _) => malformed(t"the capability listing is invalid: $reason")

      . protect:
          import Tels.Decoder.validate
          val tel = data.read[Tel]
          tel.validate(using Lira.Schemas.capabilities, Lira.Validators.registry)
          tel

    val compounds = document.childCompounds.readable.filter(_.keyword == t"capability").toVector

    val entries = compounds.map: compound =>
      val fields = compound.children.readable.flatMap(_.compounds.readable).toVector

      def field(keyword: Text): Optional[Text] =
        val values = fields.filter(_.keyword == keyword).flatMap: field =>
          field.atoms.readable.collect:
            case Tel.Atom.Inline(text, _)  => text
            case Tel.Atom.Source(text)     => text
            case Tel.Atom.Literal(_, text) => text

        values.headOption.getOrElse(Unset)

      val name = field(t"name").or(abort(malformed(t"a capability row has no name")))
      (name, field(t"version"))

    entries.zip(entries.drop(1)).foreach: (left, right) =>
      if left(0).s == right(0).s then abort(malformed(t"the capability ${left(0)} is duplicated"))
      if left(0).s > right(0).s then abort(malformed(t"capability rows are not sorted by name"))

    entries.toList.map: (name, predicate) =>
      val out = java.io.ByteArrayOutputStream()
      out.write(name.s.getBytes("UTF-8").nn)
      out.write(0)

      predicate.let: text =>
        out.write(1)
        out.write(text.s.getBytes("UTF-8").nn)

      . or(out.write(0))

      val encoding = Array.unsafeFrozen(out.toByteArray.nn)
      Atom(name, Atom.Class.Rigid, Lira.Hash(Lira.Hash.Domain.Atom(id), encoding))
