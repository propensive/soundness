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
import gossamer.*
import proscenium.compat.*
import stratiform.*
import vacuous.*

// Hand-encoded `Tels` values for the `lira` schema and its five metadata-blob schemas, following
// the precedent of `Tels.Axiom`: the Scala literals below are primary, and each mirrors a
// canonical `.tel` document (at `res/test/reliquary/`) verbatim; the test suite asserts the two
// stay in agreement and pins each schema's signature as a golden value.
//
// The schemas encode the LIRA specification's §14: worlds are `jvm | sjsir | nir | host` — the
// three universes of the motivating ecosystem, and the one world that is not a universe, which
// holds a host contract's content (hosts.md); a `Section` is keyed by world and integration
// (§9.5), may carry a `derivative` hash (its canonical derived JAR), and may carry `requires`
// records naming the host contracts its code assumes; a `version` is optional (a release
// without one is a development release) and strictly numeric; a `Dependency` may be scoped to
// particular universes or integrations, or pinned to an exact `build` during development; and a
// `Profile` records the ecosystem predicates a release claims, with the guarantee levels its
// last step did not preserve (§11.6, §12.4).
object LiraSchemas:
  import Tels.{Field, Polarity, RecordDefinition, Reference, ScalarDefinition, SelectDefinition,
      SelectRef, Struct, Type, Variant}

  import Polarity.{Implicit, Loose}

  private def field
    ( keyword:    String,
      fieldType:  Type,
      required:   Polarity = Implicit,
      repeatable: Polarity = Implicit )
  :   Field =

    Field(required, repeatable, Text(keyword), fieldType, Unset)

  private def selectRef(reference: String, required: Polarity = Implicit): SelectRef =
    SelectRef(required, Implicit, Text(reference))

  private def record(name: String, members: Tels.Member*): RecordDefinition =
    RecordDefinition(Text(name), Array.from(members), Array.empty[Text])

  private def scalar(name: String, validator: String): ScalarDefinition =
    ScalarDefinition(Text(name), Array.of(Text(validator)))

  private def select(name: String, variants: Variant*): SelectDefinition =
    SelectDefinition(Text(name), Array.from(variants), Array.empty[Text])

  private def variant(keyword: String): Variant = Variant(Text(keyword), Tels.Flag)

  private val hash:         Type = Reference(t"Hash")
  private val moduleName:   Type = Reference(t"ModuleName")
  private val namespace:    Type = Reference(t"Namespace")
  private val semver:       Type = Reference(t"Semver")
  private val natural:      Type = Reference(t"Natural")
  private val disciplineId: Type = Reference(t"DisciplineId")
  private val identifier:   Type = Reference(t"Identifier")
  private val profileId:    Type = Reference(t"ProfileId")
  private val guarantee:    Type = Reference(t"Guarantee")
  private val string:       Type = Reference(t"String")
  private val treePath:     Type = Reference(t"TreePath")

  private val hashScalar: ScalarDefinition = scalar("Hash", "base-256-hash")

  // `Tels.Reconstructor.fromTel` prefixes every reconstructed schema's scalars with the TEL
  // built-ins, so the hand-encoded values carry them identically for structural equality.
  private val builtins: Array[ScalarDefinition] = Array.of(
    scalar("Identifier", "identifier"),
    scalar("TypeName", "type-name"),
    scalar("Sigil", "sigil"),
    scalar("String", "string"))

  val lira: Tels = Tels(
    name     = t"lira",
    document = Struct(
      members = Array.of(
        field("module", moduleName),
        field("version", semver, required = Loose),
        field("lineage", hash, repeatable = Loose),
        field("toolchain", Reference(t"Tool"), repeatable = Loose),
        field("owns", namespace, required = Loose, repeatable = Loose),
        field("resource", Reference(t"Resource"), required = Loose, repeatable = Loose),
        field("api", Reference(t"Api"), repeatable = Loose),
        field("profile", Reference(t"Profile"), required = Loose, repeatable = Loose),
        field("integration", Reference(t"Integration"), required = Loose, repeatable = Loose),
        field("dependency", Reference(t"Dependency"), required = Loose, repeatable = Loose),
        field("delta", hash, required = Loose),
        field("section", Reference(t"Section"), repeatable = Loose),
        field("payload", Reference(t"Payload")),
        field("signature", Reference(t"Signature"), required = Loose, repeatable = Loose)),
      validators = Array.empty[Text]),
    layers   = Array.empty[Tels.Layer],
    sigil    = Unset,
    records  = Array.of(
      record("Tool",
        field("name", identifier),
        field("version", string),
        field("flag", identifier, required = Loose, repeatable = Loose)),

      record("Api",
        field("discipline", disciplineId),
        field("atoms", hash)),

      record("Resource",
        selectRef("ResourceMode"),
        field("path", treePath)),

      record("Profile",
        field("id", profileId),
        field("breaks", guarantee, required = Loose, repeatable = Loose)),

      record("Integration",
        field("id", identifier),
        field("rank", natural, required = Loose),
        field("label", string, required = Loose)),

      record("Dependency",
        field("module", moduleName),
        field("api", hash),
        field("version", semver, required = Loose),
        field("build", hash, required = Loose),
        field("universe", identifier, required = Loose, repeatable = Loose),
        field("integration", identifier, required = Loose, repeatable = Loose),
        field("uses", hash, required = Loose),
        field("spans", hash, required = Loose, repeatable = Loose)),

      record("Requires",
        field("module", moduleName),
        field("api", hash),
        field("version", semver, required = Loose),
        field("uses", hash, required = Loose)),

      record("Section",
        selectRef("World"),
        field("integration", identifier, required = Loose),
        field("tree", hash),
        field("delete", string, required = Loose, repeatable = Loose),
        field("derivative", hash, required = Loose),
        field("requires", Reference(t"Requires"), required = Loose, repeatable = Loose)),

      record("Payload",
        field("compression", identifier),
        field("length", natural),
        field("hash", hash)),

      record("Signature",
        field("signer", string),
        field("algorithm", identifier),
        field("key", hash),
        field("value", string))),
    scalars  = builtins ++ Array.of(
      hashScalar,
      scalar("ModuleName", "module-name"),
      scalar("Namespace", "namespace"),
      scalar("Semver", "semver"),
      scalar("Natural", "natural"),
      scalar("DisciplineId", "discipline-id"),
      scalar("ProfileId", "profile-id"),
      scalar("TreePath", "tree-path"),
      scalar("Guarantee", "guarantee")),
    selects  = Array.of(
      select("World",
        variant("jvm"),
        variant("sjsir"),
        variant("nir"),
        variant("host")),

      select("ResourceMode",
        variant("export"),
        variant("track"),
        variant("scan"))))

  val tree: Tels = Tels(
    name     = t"lira-tree",
    document = Struct(
      members    = Array.of(field("entry", Reference(t"Entry"),
          required = Loose, repeatable = Loose)),
      validators = Array.empty[Text]),
    layers   = Array.empty[Tels.Layer],
    sigil    = Unset,
    records  = Array.of(
      record("Entry",
        field("path", Reference(t"TreePath")),
        field("blob", hash))),
    scalars  = builtins ++ Array.of(hashScalar, scalar("TreePath", "tree-path")),
    selects  = Array.empty[SelectDefinition])

  val atoms: Tels = Tels(
    name     = t"lira-atoms",
    document = Struct(
      members = Array.of(
        field("discipline", disciplineId),
        field("atom", Reference(t"Atom"), required = Loose, repeatable = Loose)),
      validators = Array.empty[Text]),
    layers   = Array.empty[Tels.Layer],
    sigil    = Unset,
    records  = Array.of(
      record("Atom",
        field("class", Reference(t"AtomClass")),
        field("hash", hash),
        field("key", string))),
    scalars  = builtins ++ Array.of(
      hashScalar,
      scalar("DisciplineId", "discipline-id"),
      scalar("AtomClass", "atom-class")),
    selects  = Array.empty[SelectDefinition])

  val uses: Tels = Tels(
    name     = t"lira-uses",
    document = Struct(
      members = Array.of(
        field("module", moduleName),
        field("atom", hash, required = Loose, repeatable = Loose)),
      validators = Array.empty[Text]),
    layers   = Array.empty[Tels.Layer],
    sigil    = Unset,
    records  = Array.empty[RecordDefinition],
    scalars  = builtins ++ Array.of(hashScalar, scalar("ModuleName", "module-name")),
    selects  = Array.empty[SelectDefinition])

  val delta: Tels = Tels(
    name     = t"lira-delta",
    document = Struct(
      members = Array.of(
        field("add", hash, required = Loose, repeatable = Loose),
        field("replace", Reference(t"Replacement"), required = Loose, repeatable = Loose)),
      validators = Array.empty[Text]),
    layers   = Array.empty[Tels.Layer],
    sigil    = Unset,
    records  = Array.of(
      record("Replacement",
        field("old", hash),
        field("new", hash))),
    scalars  = builtins ++ Array.of(hashScalar),
    selects  = Array.empty[SelectDefinition])

  // The capability listing of a host contract with no formal carrier (hosts.md §5): the single
  // tree item at the path `capabilities`, claimed by `capability/1`. Rows are sorted by
  // ascending name with no duplicates; `probe` is advisory and enters no atom.
  val capabilities: Tels = Tels(
    name     = t"lira-capabilities",
    document = Struct(
      members    = Array.of(field("capability", Reference(t"Capability"),
          required = Loose, repeatable = Loose)),
      validators = Array.empty[Text]),
    layers   = Array.empty[Tels.Layer],
    sigil    = Unset,
    records  = Array.of(
      record("Capability",
        field("name", identifier),
        field("version", string, required = Loose),
        field("probe", string, required = Loose))),
    scalars  = builtins,
    selects  = Array.empty[SelectDefinition])

  // The BASE-256 schema signatures of the six canonical documents, pinned as golden values (the
  // test suite recomputes each from its `res/test/reliquary/*.tel` mirror and checks agreement).
  // A conforming document of each schema carries its signature on the pragma line.
  val liraSignature:  Text = t"εȑẀϗЫẃþyşẁЪƥdӯTzĝ8ӝήBϗĺӸḤῩῺṽOκḠôĻ"
  val treeSignature:  Text = t"ǨẙơẗỵclϋẁЫĥᾸMôĮẍOώżӯάǢЗĆӸkҚțȐωǢέӫ"
  val atomsSignature: Text = t"2ӪççÃ5AḟǑXϋƤzᾱĺHϕЂẌǒEẂẁĮί9ḀẘΊÐιЪp"
  val usesSignature:  Text = t"şşCȧOӖGҐΪḍḋjΊӁῚƟȐЌĥέȦЬƜδĻĘ1Ȑḟ6ӟÔḍ"
  val deltaSignature: Text = t"gЪΪΞKῺκḢҚdḣulƒjazỲύþῺѝgļEvḞϕϊḟẉtǣ"
  val capabilitiesSignature: Text = t"ẋƒҢιƟžŀæДNGqЌλ1ḞλſẉûÙῡẂȧώẆlώĘdSỲÔ"
