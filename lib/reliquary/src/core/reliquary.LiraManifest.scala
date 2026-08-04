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

  // The guarantee levels of §11.5 that can be claimed or broken. Behavior is absent by design:
  // no hash scheme certifies it (§18), so it is not expressible in a `breaks` field.
  enum Guarantee:
    case Linkage, Recompilation

    def keyword: Text = this match
      case Linkage       => t"linkage"
      case Recompilation => t"recompilation"

  object Guarantee:
    def parse(keyword: Text): Optional[Guarantee] = keyword.s match
      case "linkage"       => Linkage
      case "recompilation" => Recompilation
      case _               => Unset

  // An ecosystem profile this release claims to satisfy (§11.6), with the guarantee levels its
  // lineage step did not preserve (§12.4). `breaks` being empty means the step preserved every
  // level the profile certifies — the whole value of the record is that its absence means
  // something, so a level not preserved and not listed is invalid (L130).
  case class Profile(id: Text, breaks: List[Guarantee] = List())

  // One alternative dependency vector the release was built against (§9.5). `rank` orders the
  // canonical assignment (§13.3), lower first; a rank left unset sorts after every declared one,
  // so a publisher who ranks nothing gets declaration-order-independent, id-ordered resolution.
  // `label` is prose and carries no authority (§14). It is one atom even when it contains
  // spaces: `render` separates it from its keyword by a hard-space run, which switches TEL into
  // hard-space mode for the rest of the line (TEL §10.3), so the single spaces within it are
  // content rather than phrase separators.
  case class Integration(id: Text, rank: Optional[Long] = Unset, label: Optional[Text] = Unset)

  case class Dependency
    ( module:      Text,
      api:         Data,
      version:     Optional[Semver] = Unset,
      build:       Optional[Data]   = Unset,
      universe:    List[Text]       = List(),
      integration: List[Text]       = List(),
      uses:        Optional[Data]   = Unset,
      spans:       List[Data]       = List() ):

    // §13.2: the two scopes are independent and conjunctive. An empty list on either axis means
    // "every value of that axis", which is how a dependency common to all of them is declared
    // once and unscoped.
    def applies(universe: Text, integration: Optional[Text]): Boolean =
      val universeApplies = this.universe.stdlib.isEmpty || this.universe.stdlib.contains(universe)

      val integrationApplies =
        this.integration.stdlib.isEmpty
        || integration.let { id => this.integration.stdlib.contains(id) }.or(false)

      universeApplies && integrationApplies

  // How a declared resource participates in the algebra (§11.4). `Export` guarantees the name is
  // present; `Track` additionally tracks the bytes as replaceable churn; `Scan` claims a whole
  // directory atomless, so nothing under it is contractual.
  enum ResourceMode:
    case Export, Track, Scan

    def keyword: Text = this match
      case Export => t"export"
      case Track  => t"track"
      case Scan   => t"scan"

  object ResourceMode:
    def parse(keyword: Text): Optional[ResourceMode] = keyword.s match
      case "export" => Export
      case "track"  => Track
      case "scan"   => Scan
      case _        => Unset

  // One resource claim (§11.4): an authorial statement, like `owns`, that parameterizes the
  // `resource/1` discipline's claiming.
  case class Resource(mode: ResourceMode, path: TreePath)

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
        ( module      = required(fields, t"module"),
          api         = hash(required(fields, t"api")),
          version     = field(fields, t"version").let(semver(_)),
          build       = field(fields, t"build").let(hash(_)),
          universe    = List.from(repeated(fields, t"universe")),
          integration = List.from(repeated(fields, t"integration")),
          uses        = field(fields, t"uses").let(hash(_)),
          spans       = List.from(repeated(fields, t"spans").map(hash(_))) )

    val resource = top.filter(_.keyword == t"resource").map: compound =>
      val mode = texts(compound) match
        case scala.collection.immutable.Vector(mode) =>
          ResourceMode.parse(mode).or(abort(bad(t"$mode is not a resource mode")))

        case _ =>
          abort(bad(t"a resource needs exactly one mode"))

      Resource(mode, TreePath(required(children(compound), t"path")))

    val profile = top.filter(_.keyword == t"profile").map: compound =>
      val fields = children(compound)

      val breaks = repeated(fields, t"breaks").map: keyword =>
        Guarantee.parse(keyword).or(abort(bad(t"$keyword is not a guarantee level")))

      Profile(required(fields, t"id"), List.from(breaks))

    val integration = top.filter(_.keyword == t"integration").map: compound =>
      val fields = children(compound)

      Integration
        ( id    = required(fields, t"id"),
          rank  = field(fields, t"rank").let { text => text.s.toLong },
          label = field(fields, t"label") )

    val section = top.filter(_.keyword == t"section").map: compound =>
      val universe = texts(compound) match
        case scala.collection.immutable.Vector(universe) => universe

        case _ =>
          abort(bad(t"a section needs exactly one universe"))

      val fields = children(compound)

      Section
        ( universe    = universe,
          integration = field(fields, t"integration"),
          tree        = hash(required(fields, t"tree")),
          delete      = List.from(repeated(fields, t"delete").map(TreePath(_))),
          derivative  = field(fields, t"derivative").let(hash(_)) )

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
      ( module      = required(top, t"module"),
        version     = field(top, t"version").let(semver(_)),
        lineage     = List.from(repeated(top, t"lineage").map(hash(_))),
        toolchain   = List.from(toolchain),
        owns        = List.from(repeated(top, t"owns")),
        resource    = List.from(resource),
        api         = List.from(api),
        profile     = List.from(profile),
        integration = List.from(integration),
        dependency  = List.from(dependency),
        delta       = field(top, t"delta").let(hash(_)),
        section     = List.from(section),
        payload     = payload,
        signature   = List.from(signature) )

// The typed view of a `.lira` manifest (§14). Decoding always retains the parsed `Tel` alongside
// (in `Lira`): signing and reserialization operate on the TEL semantic model; this class is the
// ergonomic projection. A manifest without a `version` is a development release, identified
// purely by its hashes and unpublishable until a version is assigned.
case class LiraManifest
  ( module:      Text,
    version:     Optional[Semver]                = Unset,
    lineage:     List[Data],
    toolchain:   List[LiraManifest.Tool]         = List(),
    owns:        List[Text]                      = List(),
    resource:    List[LiraManifest.Resource]     = List(),
    api:         List[LiraManifest.Api],
    profile:     List[LiraManifest.Profile]      = List(),
    integration: List[LiraManifest.Integration]  = List(),
    dependency:  List[LiraManifest.Dependency]   = List(),
    delta:       Optional[Data]                  = Unset,
    section:     List[Section],
    payload:     LiraManifest.Payload,
    signature:   List[LiraManifest.Signature]    = List() ):

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

    resource.stdlib.foreach: resource =>
      lines += s"resource ${resource.mode.keyword}"
      lines += s"  path ${resource.path.text}"

    api.stdlib.foreach: api =>
      lines += "api"
      lines += s"  discipline ${api.discipline}"
      lines += s"  atoms ${LiraHash.text(api.atoms)}"

    profile.stdlib.foreach: profile =>
      lines += "profile"
      lines += s"  id ${profile.id}"
      profile.breaks.stdlib.foreach: level => lines += s"  breaks ${level.keyword}"

    integration.stdlib.foreach: integration =>
      lines += "integration"
      lines += s"  id ${integration.id}"
      integration.rank.let: rank => lines += s"  rank $rank"
      // Two spaces, not one: the hard-space run makes the whole remainder of the line one atom
      // (TEL §10.3), which is what lets a label contain spaces.
      integration.label.let: label => lines += s"  label  $label"

    dependency.stdlib.foreach: dependency =>
      lines += "dependency"
      lines += s"  module ${dependency.module}"
      lines += s"  api ${LiraHash.text(dependency.api)}"

      dependency.version.let: version =>
        lines += s"  version ${version.major}.${version.minor}.${version.patch}"

      dependency.build.let: build => lines += s"  build ${LiraHash.text(build)}"
      dependency.universe.stdlib.foreach: universe => lines += s"  universe $universe"

      dependency.integration.stdlib.foreach: integration =>
        lines += s"  integration $integration"

      dependency.uses.let: uses => lines += s"  uses ${LiraHash.text(uses)}"
      dependency.spans.stdlib.foreach: spans => lines += s"  spans ${LiraHash.text(spans)}"

    delta.let: delta => lines += s"delta ${LiraHash.text(delta)}"

    section.stdlib.foreach: section =>
      lines += s"section ${section.universe}"
      section.integration.let: id => lines += s"  integration $id"
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
