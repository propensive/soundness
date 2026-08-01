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
import revolution.Semver
import stratiform.*
import vacuous.*

import LiraError.Reason

object LiraManifest:
  case class Tool(name: Text, version: Text, flag: List[Text] = List())
  case class Api(discipline: Text, atoms: Data)

  case class Dependency
    ( module:   Text,
      api:      Data,
      version:  Optional[Semver] = Unset,
      build:    Optional[Data]   = Unset,
      universe: List[Text]       = List(),
      uses:     Optional[Data]   = Unset,
      spans:    List[Data]       = List() )

  case class Payload(compression: Text, length: Long, hash: Data)
  case class Signature(signer: Text, algorithm: Text, key: Data, value: Text)

  private def bad(detail: Text): LiraError =
    import errorDiagnostics.emptyDiagnostics
    LiraError(Reason.InvalidManifest(detail))

  private def texts(compound: Tel.Compound): scala.collection.immutable.Vector[Text] =
    compound.atoms.readable.collect:
      case Tel.Atom.Inline(text, _)  => text
      case Tel.Atom.Source(text)     => text
      case Tel.Atom.Literal(_, text) => text

    . toVector

  private def one(compound: Tel.Compound): Text raises LiraError =
    val atoms = texts(compound)
    if atoms.length != 1 then abort(bad(t"the ${compound.keyword} field needs exactly one atom"))
    atoms(0)

  private def hash(text: Text): Data raises LiraError =
    import errorDiagnostics.emptyDiagnostics

    mitigate:
      case Base256Error(_) => bad(t"a hash is malformed")

    . protect(Base256.decodeStrict(text))

  private def semver(text: Text): Semver raises LiraError =
    val parts = text.s.split("\\.", -1).nn
    if parts.length != 3 then abort(bad(t"the version is not `major.minor.patch`"))
    Semver(parts(0).nn.toLong, parts(1).nn.toLong, parts(2).nn.toLong)

  private def children(compound: Tel.Compound): scala.collection.immutable.Vector[Tel.Compound] =
    compound.children.readable.flatMap(_.compounds.readable).toVector

  private def field(compounds: scala.collection.immutable.Vector[Tel.Compound], keyword: Text)
  :   Optional[Text] raises LiraError =

    compounds.filter(_.keyword == keyword) match
      case scala.collection.immutable.Vector()         => Unset
      case scala.collection.immutable.Vector(compound) => one(compound)

      case _ =>
        abort(bad(t"the $keyword field appears more than once"))

  private def required(compounds: scala.collection.immutable.Vector[Tel.Compound], keyword: Text)
  :   Text raises LiraError =

    field(compounds, keyword).or(abort(bad(t"the $keyword field is missing")))

  private def repeated(compounds: scala.collection.immutable.Vector[Tel.Compound], keyword: Text)
  :   scala.collection.immutable.Vector[Text] raises LiraError =

    compounds.filter(_.keyword == keyword).flatMap: compound =>
      val atoms = texts(compound)
      if atoms.isEmpty then abort(bad(t"the $keyword field needs at least one atom"))
      atoms

  // Extracts the typed view from a type-assigned manifest document.
  def decode(tel: Tel): LiraManifest raises LiraError =
    val top = tel.childCompounds.readable.toVector

    val toolchain = top.filter(_.keyword == t"toolchain").map: compound =>
      val fields = children(compound)

      Tool
        ( required(fields, t"name"),
          required(fields, t"version"),
          List.from(repeated(fields, t"flag")) )

    val api = top.filter(_.keyword == t"api").map: compound =>
      val fields = children(compound)
      Api(required(fields, t"discipline"), hash(required(fields, t"atoms")))

    val dependency = top.filter(_.keyword == t"dependency").map: compound =>
      val fields = children(compound)

      Dependency
        ( module   = required(fields, t"module"),
          api      = hash(required(fields, t"api")),
          version  = field(fields, t"version").let(semver(_)),
          build    = field(fields, t"build").let(hash(_)),
          universe = List.from(repeated(fields, t"universe")),
          uses     = field(fields, t"uses").let(hash(_)),
          spans    = List.from(repeated(fields, t"spans").map(hash(_))) )

    val section = top.filter(_.keyword == t"section").map: compound =>
      val universe = texts(compound) match
        case scala.collection.immutable.Vector(universe) => universe

        case _ =>
          abort(bad(t"a section needs exactly one universe"))

      val fields = children(compound)

      Section
        ( universe   = universe,
          tree       = hash(required(fields, t"tree")),
          delete     = List.from(repeated(fields, t"delete").map(TreePath(_))),
          against    = List.from(repeated(fields, t"against").map(hash(_))),
          derivative = field(fields, t"derivative").let(hash(_)) )

    val payload = top.filter(_.keyword == t"payload").toList match
      case scala.List(compound) =>
        val fields = children(compound)

        Payload
          ( required(fields, t"compression"),
            required(fields, t"length").s.toLong,
            hash(required(fields, t"hash")) )

      case _ => abort(bad(t"the payload record is missing or repeated"))

    val signature = top.filter(_.keyword == t"signature").map: compound =>
      val fields = children(compound)

      Signature
        ( required(fields, t"signer"),
          required(fields, t"algorithm"),
          hash(required(fields, t"key")),
          required(fields, t"value") )

    LiraManifest
      ( module     = required(top, t"module"),
        version    = field(top, t"version").let(semver(_)),
        lineage    = List.from(repeated(top, t"lineage").map(hash(_))),
        toolchain  = List.from(toolchain),
        owns       = List.from(repeated(top, t"owns")),
        api        = List.from(api),
        dependency = List.from(dependency),
        delta      = field(top, t"delta").let(hash(_)),
        section    = List.from(section),
        payload    = payload,
        signature  = List.from(signature) )

// The typed view of a `.lira` manifest (§14). Decoding always retains the parsed `Tel` alongside
// (in `Lira`): signing and reserialization operate on the TEL semantic model; this class is the
// ergonomic projection. A manifest without a `version` is a development release, identified
// purely by its hashes and unpublishable until a version is assigned.
case class LiraManifest
  ( module:     Text,
    version:    Optional[Semver]                 = Unset,
    lineage:    List[Data],
    toolchain:  List[LiraManifest.Tool]          = List(),
    owns:       List[Text]                       = List(),
    api:        List[LiraManifest.Api],
    dependency: List[LiraManifest.Dependency]    = List(),
    delta:      Optional[Data]                   = Unset,
    section:    List[Section],
    payload:    LiraManifest.Payload,
    signature:  List[LiraManifest.Signature]     = List() ):

  // The root section is the first (§9.1); overlays materialize against it.
  def root: Optional[Section] = if section.stdlib.isEmpty then Unset else section.stdlib.head

  def development: Boolean = version.absent

  // The canonical text of the whole file's manifest part: directive, pragma, one blank line,
  // then the compounds in schema order, LF-terminated. Deterministic; `Lira.read` accepts any
  // conforming formatting, but everything reliquary writes is in this form.
  def render: Text =
    val lines = scala.collection.mutable.ArrayBuffer[String]()
    lines += "#!/usr/bin/env lira"
    lines += s"tel 1.0 ${LiraSchemas.liraSignature}"
    lines += ""
    lines += s"module $module"

    version.let: v => lines += s"version ${v.major}.${v.minor}.${v.patch}"
    lineage.stdlib.foreach: hash => lines += s"lineage ${LiraHash.text(hash)}"

    toolchain.stdlib.foreach: tool =>
      lines += "toolchain"
      lines += s"  name ${tool.name}"
      lines += s"  version ${tool.version}"
      tool.flag.stdlib.foreach: flag => lines += s"  flag $flag"

    owns.stdlib.foreach: space => lines += s"owns $space"

    api.stdlib.foreach: api =>
      lines += "api"
      lines += s"  discipline ${api.discipline}"
      lines += s"  atoms ${LiraHash.text(api.atoms)}"

    dependency.stdlib.foreach: dependency =>
      lines += "dependency"
      lines += s"  module ${dependency.module}"
      lines += s"  api ${LiraHash.text(dependency.api)}"

      dependency.version.let: version =>
        lines += s"  version ${version.major}.${version.minor}.${version.patch}"

      dependency.build.let: build => lines += s"  build ${LiraHash.text(build)}"
      dependency.universe.stdlib.foreach: universe => lines += s"  universe $universe"
      dependency.uses.let: uses => lines += s"  uses ${LiraHash.text(uses)}"
      dependency.spans.stdlib.foreach: spans => lines += s"  spans ${LiraHash.text(spans)}"

    delta.let: delta => lines += s"delta ${LiraHash.text(delta)}"

    section.stdlib.foreach: section =>
      lines += s"section ${section.universe}"
      section.against.stdlib.foreach: hash => lines += s"  against ${LiraHash.text(hash)}"
      lines += s"  tree ${LiraHash.text(section.tree)}"
      section.delete.stdlib.foreach: path => lines += s"  delete ${path.text}"
      section.derivative.let: hash => lines += s"  derivative ${LiraHash.text(hash)}"

    lines += "payload"
    lines += s"  compression ${payload.compression}"
    lines += s"  length ${payload.length}"
    lines += s"  hash ${LiraHash.text(payload.hash)}"

    signature.stdlib.foreach: signature =>
      lines += "signature"
      lines += s"  signer ${signature.signer}"
      lines += s"  algorithm ${signature.algorithm}"
      lines += s"  key ${LiraHash.text(signature.key)}"
      lines += s"  value ${signature.value}"

    Text(lines.mkString("", "\n", "\n"))
