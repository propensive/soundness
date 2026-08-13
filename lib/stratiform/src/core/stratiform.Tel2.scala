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
package stratiform

import scala.collection.immutable.Vector

import scala.caps

import proscenium.compat.*

import scala.collection.Factory
import scala.compiletime.*

import adversaria.*
import anticipation.*
import aperture.*
import contextual.*
import contingency.*
import denominative.*
import distillate.*
import gossamer.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*
import wisteria.*

// Phase-2 encode/decode typeclasses and Wisteria derivation. Mirrors
// jacinta.Json2 / DecodableDerivation / EncodableDerivation but produces
// TEL presentation structures instead of JSON ASTs.
//
// Encoding model: a scalar value (Text, Int, ...) produces a Tel wrapping
// a single Compound with one inline atom carrying the text. A product
// type (case class) produces a Tel wrapping a Compound whose children
// list contains one Compound per field, keyed by the field's label.
// Decoding inverts these mappings.

// A present entry with a keyword and no children — the node form under which
// a bare compound line reaches a scalar or flag decoder. The keywordless
// empty `Tel` (the historical absent-field fallback, and `Tel.empty`) is
// excluded, so absence keeps raising `Absent`.
private[stratiform] def bareCompound(tel: Tel): Boolean = tel.subtree match
  case c: Tel.Compound => c.keyword != t"" && tel.childCompounds.nil
  case _               => false

// Register a decode error and continue with `sentinel` instead of aborting, so
// that sibling fields of a product can each accrue their own error under a
// `validate[Tel.Focus]` boundary. A field with no atom (the empty `Tel` handed
// to a primitive by `conjunction` for an absent field) raises `Absent`; an atom
// that fails to parse raises `NotScalar`, distinguishing "missing" from
// "wrong shape". Outside a `validate` boundary the ambient `ThrowTactic` makes
// `raise` throw, preserving fail-fast decoding.
// At file level (not a trait member) so the decoder SAMs that call it capture only their
// tactic, not the enclosing `Tel2` instance.
private[stratiform] def primitiveFault[value]
  ( tel: Tel, expected: Text, sentinel: value )
  ( parse: Text => Optional[value] )
  ( using Tactic[Tel.Error] )
:   value =

  if tel.atomTexts.isEmpty then raise(Tel.Error(Tel.Error.Reason.Absent)) yet sentinel
  else parse(tel.primaryAtom).or:
    raise(Tel.Error(Tel.Error.Reason.NotScalar(tel.primaryAtom, expected))) yet sentinel

trait Tel2 extends Tel3:
  // The read-only `Openable` instance: any source parseable as `Tel` may
  // be opened, but a `Write` mode is refused at open time. Lower
  // priority than `Tel.telOpenable`, which wins whenever the source also
  // supports write-back.
  given telViewOpenable: [source]
  =>  ( readable: (source is Readable to Tel)^,
        mutationError: Tactic[Mutation.Error] )
  =>  (Tel.ViewOpenable[source]^{readable, mutationError}) =
    Tel.ViewOpenable[source]()

  // `tel"…"` interpolator: parses at compile time and substitutes typed
  // holes via Encodable in Tel.
  inline given interpolator: Tel is Interpolable:
    type Result = Tel

    transparent inline def interpolate[parts <: Tuple, origins <: Tuple]
      ( inline insertions: Any* )
    :   Tel =

      ${stratiform.internal.interpolator[parts, origins]('insertions)}

  // `tel"…"` extractor: parses the pattern at compile time and produces
  // a structural matcher that binds atom-text captures.
  inline given extrapolator: Tel is Extrapolable:
    transparent inline def extrapolate[parts <: Tuple, origins <: Tuple](scrutinee: Tel)
    :   Boolean | Option[Tuple | Tel] =

      ${stratiform.internal.extractor[parts, origins]('scrutinee)}

  inline given decodable: [value] => value is Tel.Decodable = summonFrom:
    case given (`value` is Decodable in Text) =>
      Tel.Decodable(() => Morphology.Str, Tel.Nature.Scalar):
        provide[Tactic[Tel.Error]](_.primaryAtom.as[value])

    case given Reflection[`value`] => DecodableDerivation.derived

  inline given encodable: [value] => value is Tel.Encodable = summonFrom:
    case given (`value` is Encodable in Text) =>
      Tel.Encodable(() => Morphology.Str, Tel.Nature.Scalar): v => Tel.scalar(v.encode)

    case given Reflection[`value`] => EncodableDerivation.derived

  // ── Direct parsing: element-wise `Tel.Field` instances and the read path ──

  // Element-wise `Tel.Field` for collections, resolved during derivation:
  // the element's own parser comes from the fallback chain, so nested
  // products still parse directly. This layer beats `Tel3`'s fallback, so
  // collection types never reach its `Reflection` case (a `List`'s own
  // `Mirror` would otherwise derive it as a sum). The instance is
  // repeatable: the product engine gathers every same-keyword occurrence,
  // exactly as the AST derivation collects all matching compounds.
  given fieldCollection: [collection <: Iterable, element]
  =>  ( factory: Factory[element, collection[element]],
        tactic:  Tactic[Tel.Error] )
  =>  ( field: => (element is Tel.Field)^ )
  =>  collection[element] is Tel.Field =
    Tel.Field(Tel.Parsable.iterable[collection, element](field))

  // Alias counterparts: the opaque prelude collections do not conform to
  // `Iterable`, so each gets its own instance built at the underlying stdlib
  // type and cast (a no-op at erasure).
  given fieldList: [list <: List, element]
  =>  ( tactic: Tactic[Tel.Error] )
  =>  ( field: => (element is Tel.Field)^ )
  =>  list[element] is Tel.Field =
    Tel.Field(Tel.Parsable.iterable[scala.collection.immutable.List, element](field))
    . asInstanceOf[list[element] is Tel.Field]

  given fieldSet: [set <: Set, element]
  =>  ( tactic: Tactic[Tel.Error] )
  =>  ( field: => (element is Tel.Field)^ )
  =>  set[element] is Tel.Field =
    Tel.Field(Tel.Parsable.iterable[scala.collection.immutable.Set, element](field))
    . asInstanceOf[set[element] is Tel.Field]

  given fieldSeries: [sequence <: Sequence, element]
  =>  ( tactic: Tactic[Tel.Error] )
  =>  ( field: => (element is Tel.Field)^ )
  =>  sequence[element] is Tel.Field =
    Tel.Field(Tel.Parsable.iterable[Vector, element](field))
    . asInstanceOf[sequence[element] is Tel.Field]

  // Element-wise `Tel.Field` for `Optional`, resolved during derivation:
  // the inner instance comes from the field fallback chain (by-name, so
  // recursive types resolve, exactly like collections), and the wrapper's
  // semantics mirror the AST `optionalDecodable`: an entry with neither an
  // inline atom nor a child compound reads as `Unset`, as does a missing
  // keyword. Above the blanket so an `Optional` field resolves here with
  // the same specificity preference the AST derivation's `Tel.Decodable`
  // summon exhibits.
  given fieldOptional: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( tactic: Tactic[Tel.Error] )
  =>  ( field: => (inner is Tel.Field)^ )
  =>  value is Tel.Field =
    Tel.Field(Tel.Parsable.optionality[inner, value](field))

  // The AST-materializing read path: `source.read[Foo in Tel]` shorthand for
  // `source.read[Tel].as[Foo]`. Mirrors `jacinta`'s `aggregableDirect` for
  // `value in Json`. The `Form` type-tag is added by an `asInstanceOf` cast —
  // `value in Tel` is just `value { type Form = Tel }` so the cast is a no-op
  // at runtime. Lives at this priority so `object Tel`'s direct-parsing
  // `aggregableParsed` wins whenever the value has a `Tel.Parsable`; when it
  // does not (all pre-`Parsable` code), this resolves exactly as before.
  given aggregableIn: [value: distillate.Decodable in Tel]
  =>  (tactic: Tactic[Tel.Error], tracking: zephyrine.PositionTracking)
  =>  (((value in Tel) is Aggregable by Data)^{tactic}) =
    source => Tel.parseTracking(Tel.concatenate(source)).as[value].asInstanceOf[value in Tel]

  object ParsableDerivation extends Derivable[Tel.Field]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Tel.Field =

      // Like `DecodableDerivation.conjunction`: the capabilities are summoned
      // at the derivation site and the instance is sealed per the codec-thunk
      // pattern. A single `contexts` traversal collects, per field, its wire
      // keyword (`@name[Tel]`-aware, camel→kebab otherwise — the same mapping
      // as the AST derivation), its parser (via the `Field` fallback chain)
      // and its declared default; `Tel.Parsable.product` owns the entry loop,
      // so no per-field lambda ever closes over the reader.
      caps.unsafe.unsafeAssumePure:
        val reflection = infer[ProductReflection[derivation]]

        Tel.Parsable.product[derivation](
          { () =>
            val renames: Map[Text, Text] = relabelling[derivation, Tel]

            contexts[derivation]():
              [field] => context =>
                ( renames(label).or(Tel.camelToKebab(label.s)).s,
                  context: Tel.Parsing,
                  default[Optional[field]]: Any )
          },
          values => Tel.Parsable.assemble(reflection, values))
          ( using infer[Foci[Tel.Focus]], infer[Tactic[Tel.Error]] )

    inline def disjunction[derivation: SumReflection]: derivation is Tel.Field =
      // A sum's wire form is a single child compound keyed by the variant's
      // name, which cannot be recognised before the entry itself is read —
      // so a sum always takes the AST bridge over its derived (or custom)
      // decoder, keeping the two paths identical by construction. Sealed per
      // the codec-thunk pattern: the instance captures a resolution-scoped
      // decoder.
      caps.unsafe.unsafeAssumePure:
        Tel.Field(Tel.Parsable.fromDecodable(infer[derivation is Tel.Decodable]))

  object DecodableDerivation extends Derivable[Tel.Decodable]:
    // The per-field decode, shared by the fail-fast and accruing traversals of `conjunction` —
    // a plain method, NOT part of the inline expansion: inlining this body twice per derivation
    // (once per traversal) pushed classes with many derived decoders over the JVM class-size
    // limit.
    private def decodeField[field]
      ( ctx:        (Tel.Decodable { type Self = field })^,
        telVal:     Tel,
        keyword:    Text,
        positional: scala.collection.immutable.List[Tel.Atom],
        fallback:   Optional[field] )
      ( using tactic: Tactic[Tel.Error] )
    :   field =

      given fulminate.Diagnostics = tactic.diagnostics

      // A `List`/`Set` field (`ctx.repeatable`) is encoded as repeated
      // keyword compounds, so gather them all into a Document for the
      // collection decoder — positionally-assigned atoms first, since
      // atoms precede children (§18.3 step 4). Every other field —
      // scalar, nested product, `Optional`, `Map` (a single `entries`
      // compound) — reads one match.
      if ctx.repeatable then
        val compounds =
          if positional.isEmpty
          then telVal.childCompounds.filter(_.keyword == keyword)
          else
            val buffer = scala.collection.mutable.ArrayBuffer.empty[Tel.Compound]

            // A flag's atom is its keyword, meaning presence: it
            // becomes a bare compound, not an atom-bearing one.
            positional.foreach: atom =>
              buffer +=
                ( if ctx.nature == Tel.Nature.Flag
                  then Tel.Compound(keyword, Array.empty, Unset, Array.empty)
                  else Tel.Compound(keyword, Array.of(atom), Unset, Array.empty) )

            telVal.childCompounds.each: compound =>
              if compound.keyword == keyword then buffer += compound

            Array.from(buffer)

        ctx.decoded:
          Tel.make
            ( Tel.Document
              ( Unset, Unset, Tel.LineEndings.Lf,
               Array.of(Tel.Block(Array.empty, Unset, compounds, 0)) ) )
      else
        val match0 = telVal.field(keyword)

        if positional.isEmpty then
          match0.lay(fallback.or(ctx.absent()))(ctx.decoded(_))
        else
          // §20.2 step 5c: an inline atom plus a same-keyword
          // child fills a non-repeatable member twice (E308).
          // The atom wins — atoms precede children.
          if match0.present
          then raise(Tel.Error(Tel.Error.Reason.NonRepeatableTooMany))

          ctx.decoded:
            Tel.make:
              // A flag's atom is its keyword, meaning presence:
              // it becomes a bare compound.
              if ctx.nature == Tel.Nature.Flag
              then Tel.Compound(keyword, Array.empty, Unset, Array.empty)
              else Tel.Compound(keyword, Array.of(positional.head), Unset, Array.empty)

    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Tel.Decodable =

      // `@name[Tel]` / bare `@name` renames and the per-field positional
      // profiles are per-derivation constants, hoisted out of the decode
      // call (lazily, so recursive types tie their knot).
      lazy val renames: Map[Text, Text] = relabelling[derivation, Tel]

      lazy val profiles: Array[Positional.Profile]^{} =
        contexts[derivation]():
          [field] => context =>
            // A Flag field is never required: its absence decodes `false`,
            // so the skip rule may pass over it — which the canonical
            // encoder relies on when it elides a false flag from a run.
            Positional.Profile
              ( renames(label).or(Tel.camelToKebab(label.s)),
                context.nature,
                context.repeatable,
                required = context.nature != Tel.Nature.Flag
                           && !(context.optional || default[Optional[field]].present) )

      // The object `Morphology` is built from the field decoders' own shapes (a single
      // inlined `contexts` traversal — kept here, not factored out, so it does not
      // perturb the `build` traversal), keeping a fused `Decodable & Schematic`
      // coherent. Built by-name so recursive types compile.
      Tel.Decodable({ () =>
        val fields: List[(Text, Morphology)] =
          contexts[derivation](): [field] => context => (label, context.shape())
          . toList // direct shim, not `to[List]`: inline re-elaboration freshens the array

        Morphology.Obj(fields, fields.collect { case (label, shape) if !shape.optional => label })
      }):
        telVal =>
          provide[Foci[Tel.Focus]]:
            provide[Tactic[Tel.Error]]:
              // §19.2 positional pre-pass (issue #1694): the compound's own
              // atoms fill fields in declaration order, per the schema-free
              // §20.2 step 3. The dominant wire form has no atoms and skips
              // the pass entirely.
              val atoms = telVal.atoms

              val assigned: Array[List[Tel.Atom]]^{} =
                if atoms.length == 0 then Array.empty else Positional.assign(atoms, profiles)

              val foci = infer[Foci[Tel.Focus]]

              if !foci.active then
                // Fail-fast path: the first recorded error escapes through the tactic, so
                // decode and construct in a single traversal, with no venture slots and no
                // focus bookkeeping.
                build[derivation]: [field] =>
                  ctx =>
                    val keyword: Text = renames(label).or(Tel.camelToKebab(label.s))

                    val positional: scala.collection.immutable.List[Tel.Atom] =
                      if assigned.length == 0 then scala.collection.immutable.Nil
                      else assigned(index).stdlib

                    decodeField[field](ctx, telVal, keyword, positional, default[Optional[field]])

              else
                // Accruing path (as in jacinta's `decodeObject`): decode EVERY field into a
                // venture slot first, marking a slot failed if its decode registered any focus,
                // and construct only when all slots are clean — user constructor code never
                // sees a garbage fallback value. On failure the returned value is never used
                // (the caller's scope is tainted), and siblings keep accruing in the meantime.
                val slots: Array[Venture[Any]]^{} =
                  contexts[derivation]()[Venture[Any]]: [field] =>
                    ctx =>
                      val keyword: Text = renames(label).or(Tel.camelToKebab(label.s))

                      val positional: scala.collection.immutable.List[Tel.Atom] =
                        if assigned.length == 0 then scala.collection.immutable.Nil
                        else assigned(index).stdlib

                      // Tag every error registered while decoding this field with its
                      // keyword path, so that under a `validate[Tel.Focus]` boundary the
                      // primitives' `raise … yet sentinel` accrue per-field rather than the
                      // first malformed field aborting the whole record.
                      focus({
                        val base = prior.let(_.pointer).or(TelPath.Root)
                        Tel.Focus(base.prepend(keyword))
                      }):
                        val before = foci.length

                        val value: field =
                          decodeField[field]
                            (ctx, telVal, keyword, positional, default[Optional[field]])

                        if foci.length > before then Venture.failed else Venture(value)

                var failed = false
                var slot = 0

                while slot < slots.length do
                  if !slots.readUnchecked(slot).ready then failed = true
                  slot += 1

                if failed then null.asInstanceOf[derivation]
                else
                  build[derivation]: [field] =>
                    ctx => slots.readUnchecked(index).asInstanceOf[Venture[field]].vouch

    inline def disjunction[derivation: SumReflection]: derivation is Tel.Decodable =
      // A sum is a document whose single child compound is the chosen variant, keyed by
      // the variant's (kebab-cased) name. Dispatch on that child's keyword and decode it
      // as the variant. This is the select-member form `Tel.Type.assign` and BinTEL key
      // on. The codec-carried shape stays permissive (`Any`) — walking the variants
      // (`delegate`) is `fallible` and would leak a `Tactic[Variant.Error]` requirement
      // onto every codec; the precise select schema comes from the standalone
      // `Schematic` / `Tels.tels`.
      // Kebab keyword → variant label (the label `delegate` dispatches on), a
      // per-derivation constant: built once here rather than on every decode
      // call, whose profile it dominated (map building plus generic-equality
      // lookups, per occurrence) — jacinta's map hoist.
      val labels: Map[Text, Text] =
        Map.from:
          variantLabels.stdlib.map: label => Tel.camelToKebab(label.s) -> label

      Tel.Decodable(() => Morphology.Any):
        telVal =>
          provide[Foci[Tel.Focus]]:
            provide[Tactic[Tel.Error]]:
              provide[Tactic[Variant.Error]]:
                val compounds = telVal.childCompounds

                // A sum position with no child compound carries no variant to
                // dispatch on: a decode-layer absence, not a crash. Under an
                // accruing scope, record ONE error and skip the variant decode
                // without killing the whole scope: the returned value is never
                // used (the caller sees the focus delta, or the tracking scope
                // is tainted), so siblings keep accruing. Fail-fast scopes
                // abort as before.
                if compounds.nil then
                  if infer[Foci[Tel.Focus]].active
                  then raise(Tel.Error(Tel.Error.Reason.Absent)) yet null.asInstanceOf[derivation]
                  else abort(Tel.Error(Tel.Error.Reason.Absent))
                else
                  val variant: Tel = Tel.make(compounds.head)
                  val variantKeyword: Text = labels(variant.keyword).or(variant.keyword)

                  delegate(variantKeyword): [variant <: derivation] =>
                    ctx => ctx.decoded(variant)

  object EncodableDerivation extends Derivable[Tel.Encodable]:
    inline def conjunction[derivation <: Product: ProductReflection]
    :   derivation is Tel.Encodable =

      // `@name[Tel]` / bare `@name` renames: field name -> keyword, used
      // verbatim; an unannotated field falls back to its camel→kebab form.
      lazy val renames: Map[Text, Text] = relabelling[derivation, Tel]

      new Tel.Encodable:
        type Self = derivation

        def shape(): Morphology =
          val fields: List[(Text, Morphology)] =
            contexts[derivation](): [field] => context => (label, context.shape())
            . toList // direct shim, not `to[List]`: inline re-elaboration freshens the array

          Morphology.Obj(fields, fields.collect { case (label, shape) if !shape.optional => label })

        // The encoded value is taken FROZEN (`^{}`) rather than as a bare `derivation`. The type
        // parameter may capture, so writing it plainly here re-elaborates it with a fresh capture
        // set, which no longer matches the `Self` of `Encodable.encoded` — the definition then
        // fails its override check under capture checking. A frozen parameter is no restriction:
        // an encoder reads its argument and retains nothing of it.
        def encoded(value: derivation^{}): Tel =
          val compounds = scala.collection.mutable.ArrayBuffer.empty[Tel.Compound]

          fields(value): [field] =>
            fieldValue =>
              val encoded = contextual.encode(fieldValue)
              val keyword = renames(label).or(Tel.camelToKebab(label.s))

              encoded.subtree match
                case c: Tel.Compound =>
                  compounds += c.copy(keyword = keyword)

                // A list/set field encodes to a Document of element compounds;
                // flatten them as repeated fields, each re-keyed to the field label
                // (TEL's representation of a repeated field — see `#1291`).
                case d: Tel.Document =>
                  d.children.each: child =>
                    child.compounds.each: compound =>
                      compounds += compound.copy(keyword = keyword)

          Tel.compound(t"", Array.empty, Array.from(compounds))

        // The §22.2 member description of a value, in field order — the
        // input to `Mutation.construct` for the canonical forms below.
        private def membersOf(value: derivation^{})
        :   scala.collection.immutable.List[Mutation.Member] =

          val members = scala.collection.mutable.ListBuffer.empty[Mutation.Member]

          fields(value): [field] =>
            fieldValue =>
              val keyword: Text = renames(label).or(Tel.camelToKebab(label.s))

              contextual.nature match
                case Tel.Nature.Flag =>
                  // No derived codec is flag-natured (a Scala `Boolean` is a
                  // value, §20 flags are keyword presence), but a custom one
                  // may be: `true` is the bare keyword, `false` is elided.
                  val encoded = contextual.encode(fieldValue)

                  if encoded.primaryAtom == t"true" then members += Mutation.Member.Flag(keyword)
                  else members += Mutation.Member.Break

                case Tel.Nature.Scalar =>
                  val encoded = contextual.encode(fieldValue)

                  encoded.subtree match
                    case c: Tel.Compound =>
                      members += Mutation.Member.Value(keyword, List(encoded.primaryAtom))

                    case d: Tel.Document =>
                      // A repeatable scalar field: all occurrences inline or
                      // none (§22.2). A single occurrence still terminates
                      // the run — a repeatable member holds its atom
                      // position and would consume any atom that followed —
                      // so it takes the child form. An empty collection (or
                      // Unset Optional) contributes nothing but breaks the
                      // run, exactly like an absent Scalar member.
                      val children = d.children.bind(_.compounds)

                      if children.length == 0 then members += Mutation.Member.Break
                      else if children.length == 1
                      then members += Mutation.Member.Child(children.readUnchecked(0).copy(keyword = keyword))
                      else
                        val texts = scala.collection.mutable.ListBuffer.empty[Text]

                        children.each: child =>
                          texts +=
                            ( if child.atoms.length == 0 then t""
                              else Positional.text(child.atoms.readUnchecked(0)) )

                        members += Mutation.Member.Value(keyword, List.of(texts.toList))

                case Tel.Nature.Struct =>
                  val encoded = contextual.constructed(fieldValue)

                  encoded.subtree match
                    case c: Tel.Compound =>
                      members += Mutation.Member.Child(c.copy(keyword = keyword))

                    case d: Tel.Document =>
                      val children = d.children.bind(_.compounds)

                      if children.length == 0 then members += Mutation.Member.Break
                      else children.each: child =>
                        members += Mutation.Member.Child(child.copy(keyword = keyword))

          members.toList

        // The canonical child form: this record's compound with its §22.2
        // leading inline run, for embedding under a keyword.
        override def constructed(value: derivation^{}): Tel =
          Tel.make(Mutation.construct(t"", List.of(membersOf(value)), '#'))

        // The canonical document form: the root carries no atoms (§20.2),
        // so a leading `Break` suppresses the root's own run while nested
        // records keep theirs.
        override def canonicalized(value: derivation^{}): Tel =
          val compound =
            Mutation.construct(t"", List.of(Mutation.Member.Break :: membersOf(value)), '#')

          Tel.make(Tel.Document(Unset, Unset, Tel.LineEndings.Lf, compound.children))

    inline def disjunction[derivation: SumReflection]: derivation is Tel.Encodable =
      // A sum encodes as a document whose single child compound is the chosen variant,
      // keyed by the variant's (kebab-cased) name with the variant's fields as its own
      // children. This is the select-member form `Tel.Type.assign` and BinTEL key on (the
      // variant is a member of the document, matched by the schema's `SelectRef`), and it
      // round-trips identically to the same document parsed from text. The codec-carried
      // shape stays permissive (`Any`); the precise select schema comes from the standalone
      // `Schematic` / `Tels.tels`.
      new Tel.Encodable:
        type Self = derivation
        def shape(): Morphology = Morphology.Any

        // Frozen, as in `conjunction` above.
        def encoded(value: derivation^{}): Tel =
          variant(value): [variant <: derivation] =>
            v =>
              val keyword: Text = Tel.camelToKebab(label.s)

              contextual.encode(v).subtree match
                case compound: Tel.Compound =>
                  Tel.compound(t"", Array.empty, Array.of(compound.copy(keyword = keyword)))

                case other =>
                  Tel.make(other)

        // The canonical child form: the chosen variant in its own §22.2
        // construct form, so a record variant carries its inline run.
        override def constructed(value: derivation^{}): Tel =
          variant(value): [variant <: derivation] =>
            v =>
              val keyword: Text = Tel.camelToKebab(label.s)

              contextual.constructed(v).subtree match
                case compound: Tel.Compound =>
                  Tel.compound(t"", Array.empty, Array.of(compound.copy(keyword = keyword)))

                case other =>
                  Tel.make(other)

  // Primitive instances: Text/Int/Long/Double/Boolean as Compound + inline
  // atom. These mirror jacinta.Json's primitive decoders but go through
  // the atom text rather than a JSON AST.

  // A present, keyword-bearing compound with no atom is the empty string
  // (§18.3/§20.2 step 1: a Scalar's value is its atom of any form, or the
  // empty string if it has none); a keywordless empty node — the historical
  // absent-field fallback — still raises `Absent`.
  given textDecodable: (tactic: Tactic[Tel.Error]) => ((Text is Tel.Decodable)^{tactic}) =
    Tel.Decodable(() => Morphology.Str, Tel.Nature.Scalar): tel =>
      if tel.atomTexts.isEmpty && bareCompound(tel) then t""
      else primitiveFault(tel, t"Text", t""): atom => atom

  given stringDecodable: (tactic: Tactic[Tel.Error]) => ((String is Tel.Decodable)^{tactic}) =
    Tel.Decodable(() => Morphology.Str, Tel.Nature.Scalar): tel =>
      if tel.atomTexts.isEmpty && bareCompound(tel) then ""
      else primitiveFault(tel, t"String", ""): atom => atom.s

  given intDecodable: (tactic: Tactic[Tel.Error]) => ((Int is Tel.Decodable)^{tactic}) =
    Tel.Decodable(() => Morphology.Whole, Tel.Nature.Scalar): tel =>
      primitiveFault(tel, t"Int", 0): atom =>
        try atom.s.toInt catch case _: NumberFormatException => Unset

  given longDecodable: (tactic: Tactic[Tel.Error]) => ((Long is Tel.Decodable)^{tactic}) =
    Tel.Decodable(() => Morphology.Whole, Tel.Nature.Scalar): tel =>
      primitiveFault(tel, t"Long", 0L): atom =>
        try atom.s.toLong catch case _: NumberFormatException => Unset

  given doubleDecodable: (tactic: Tactic[Tel.Error]) => ((Double is Tel.Decodable)^{tactic}) =
    Tel.Decodable(() => Morphology.Real, Tel.Nature.Scalar): tel =>
      primitiveFault(tel, t"Double", 0.0): atom =>
        try atom.s.toDouble catch case _: NumberFormatException => Unset

  // A Scala `Boolean` is a value, not a TEL flag (§20 flags are keyword
  // presence alone), so it reads and writes the explicit `true`/`false` atom
  // — the mapping every layer agrees on, from the derived schema through
  // BinTEL. A codec may opt into `Tel.Nature.Flag` for a genuinely
  // flag-shaped type.
  given booleanDecodable: (tactic: Tactic[Tel.Error]) => ((Boolean is Tel.Decodable)^{tactic}) =
    Tel.Decodable(() => Morphology.Bool, Tel.Nature.Scalar): tel =>
      primitiveFault(tel, t"Boolean", false): atom =>
        atom.s match
          case "true"  => true
          case "false" => false
          case _       => Unset

  given telDecodable: Tel is Tel.Decodable = Tel.Decodable(() => Morphology.Any)(identity(_))

  given textEncodable: Text is Tel.Encodable =
    Tel.Encodable(() => Morphology.Str, Tel.Nature.Scalar): text => Tel.scalar(text)

  given stringEncodable: String is Tel.Encodable =
    Tel.Encodable(() => Morphology.Str, Tel.Nature.Scalar): s => Tel.scalar(Text(s))

  given intEncodable: Int is Tel.Encodable =
    Tel.Encodable(() => Morphology.Whole, Tel.Nature.Scalar): i => Tel.scalar(Text(i.toString))

  given longEncodable: Long is Tel.Encodable =
    Tel.Encodable(() => Morphology.Whole, Tel.Nature.Scalar): l => Tel.scalar(Text(l.toString))

  given doubleEncodable: Double is Tel.Encodable =
    Tel.Encodable(() => Morphology.Real, Tel.Nature.Scalar): d => Tel.scalar(Text(d.toString))

  given booleanEncodable: Boolean is Tel.Encodable =
    Tel.Encodable(() => Morphology.Bool, Tel.Nature.Scalar): b => Tel.scalar(Text(b.toString))

  given telEncodable: Tel is Tel.Encodable = Tel.Encodable(() => Morphology.Any)(identity(_))

  // Optional / List support — repeatable scalar fields produce multiple
  // compounds with the same keyword; we return a Document-rooted Tel
  // containing the list elements as siblings, which the product encoder
  // recognises and flattens with the field's label.

  given optionalEncodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  ( encodable: inner is Tel.Encodable )
  =>  value is Tel.Encodable =
    new Tel.Encodable:
      type Self = value
      def shape(): Morphology = Morphology.Opt(encodable.shape())
      override def nature: Tel.Nature = encodable.nature
      override def optional: Boolean = true

      def encoded(opt: value): Tel =
        opt.let(_.asInstanceOf[inner]).lay(emptyDocument)(encodable.encoded(_))

      override def constructed(opt: value): Tel =
        opt.let(_.asInstanceOf[inner]).lay(emptyDocument)(encodable.constructed(_))

  given optionalDecodable: [inner <: value, value >: Unset.type: Mandatable to inner]
  =>  Tactic[Tel.Error]
  =>  ( decodable0: -> (inner is Tel.Decodable) )
  =>  value is Tel.Decodable =
    new Tel.Decodable:
      type Self = value
      def shape(): Morphology = Morphology.Opt(decodable0.shape())
      override def nature: Tel.Nature = decodable0.nature
      override def optional: Boolean = true
      override def absent()(using Tactic[Tel.Error]): value = Unset

      def decoded(telVal: Tel): value =
        if telVal.childCompounds.nil && telVal.atomTexts.nil then Unset
        else decodable0.decoded(telVal)

  // Collection support (aligned with `#1291`) — a `List`/`Set` encodes to a
  // Document-rooted Tel whose children are the elements' compounds; the product
  // encoder (`conjunction`) flattens those into repeated fields, each re-keyed to
  // the field's label (TEL's representation of a repeated field). Decoding inverts
  // this: the product decoder gathers all sibling compounds sharing the field's
  // keyword into a Document and hands it here, where each child is decoded as an
  // element via the target's `Factory`.

  // Re-keys an encoded value's compound (or wraps a document) under `keyword`.
  given listEncodable: [list <: List, element] => (encodable: -> (element is Tel.Encodable))
  =>  list[element] is Tel.Encodable =
    new Tel.Encodable:
      type Self = list[element]
      def shape(): Morphology = Morphology.Arr(encodable.shape())
      override def nature: Tel.Nature = encodable.nature
      override def optional: Boolean = true
      def encoded(values: list[element]): Tel = collectionDocument(values.stdlib)(using encodable)

      override def constructed(values: list[element]): Tel =
        constructedDocument(values.stdlib)(using encodable)

  given setEncodable: [set <: Set, element] => (encodable: -> (element is Tel.Encodable))
  =>  set[element] is Tel.Encodable =
    new Tel.Encodable:
      type Self = set[element]
      def shape(): Morphology = Morphology.Arr(encodable.shape())
      override def nature: Tel.Nature = encodable.nature
      override def optional: Boolean = true
      def encoded(values: set[element]): Tel = collectionDocument(values.stdlib)(using encodable)

      override def constructed(values: set[element]): Tel =
        constructedDocument(values.stdlib)(using encodable)

  given seriesEncodable: [sequence <: Sequence, element] => (encodable: -> (element is Tel.Encodable))
  =>  sequence[element] is Tel.Encodable =
    new Tel.Encodable:
      type Self = sequence[element]
      def shape(): Morphology = Morphology.Arr(encodable.shape())
      override def nature: Tel.Nature = encodable.nature
      override def optional: Boolean = true

      def encoded(values: sequence[element]): Tel =
        collectionDocument(values.stdlib)(using encodable)

      override def constructed(values: sequence[element]): Tel =
        constructedDocument(values.stdlib)(using encodable)

  given collectionDecodable: [collection <: Iterable, element]
  =>  ( factory:   Factory[element, collection[element]],
        element0:  -> (element is Tel.Decodable) )
  =>  Tactic[Tel.Error]
  =>  collection[element] is Tel.Decodable =
    new Tel.Decodable:
      type Self = collection[element]
      def shape(): Morphology = Morphology.Arr(element0.shape())
      override def repeatable: Boolean = true
      override def nature: Tel.Nature = element0.nature
      override def optional: Boolean = true

      def decoded(telVal: Tel): collection[element] =
        val builder = factory.newBuilder

        telVal.subtree.absolve match
          case document: Tel.Document =>
            document.children.bind(_.compounds).each: compound =>
              builder += element0.decoded(Tel.make(compound))

          case compound: Tel.Compound =>
            builder += element0.decoded(telVal)

        builder.result()

  // Alias counterparts: the opaque prelude collections do not conform to
  // `Iterable`, so each decodes at the underlying stdlib type and casts. The
  // loop is inlined (rather than delegating to a shared helper) so nothing
  // captures `Tel2.this`, mirroring `collectionDecodable` above.
  given listDecodable: [list <: List, element]
  =>  ( element0: -> (element is Tel.Decodable) )
  =>  Tactic[Tel.Error]
  =>  list[element] is Tel.Decodable =
    new Tel.Decodable:
      type Self = list[element]
      def shape(): Morphology = Morphology.Arr(element0.shape())
      override def repeatable: Boolean = true
      override def nature: Tel.Nature = element0.nature
      override def optional: Boolean = true

      def decoded(telVal: Tel): list[element] =
        val builder = scala.collection.immutable.List.newBuilder[element]

        telVal.subtree.absolve match
          case document: Tel.Document =>
            document.children.bind(_.compounds).each: compound =>
              builder += element0.decoded(Tel.make(compound))

          case compound: Tel.Compound =>
            builder += element0.decoded(telVal)

        builder.result().asInstanceOf[list[element]]

  given setDecodable: [set <: Set, element]
  =>  ( element0: -> (element is Tel.Decodable) )
  =>  Tactic[Tel.Error]
  =>  set[element] is Tel.Decodable =
    new Tel.Decodable:
      type Self = set[element]
      def shape(): Morphology = Morphology.Arr(element0.shape())
      override def repeatable: Boolean = true
      override def nature: Tel.Nature = element0.nature
      override def optional: Boolean = true

      def decoded(telVal: Tel): set[element] =
        val builder = scala.collection.immutable.Set.newBuilder[element]

        telVal.subtree.absolve match
          case document: Tel.Document =>
            document.children.bind(_.compounds).each: compound =>
              builder += element0.decoded(Tel.make(compound))

          case compound: Tel.Compound =>
            builder += element0.decoded(telVal)

        builder.result().asInstanceOf[set[element]]

  given seriesDecodable: [sequence <: Sequence, element]
  =>  ( element0: -> (element is Tel.Decodable) )
  =>  Tactic[Tel.Error]
  =>  sequence[element] is Tel.Decodable =
    new Tel.Decodable:
      type Self = sequence[element]
      def shape(): Morphology = Morphology.Arr(element0.shape())
      override def repeatable: Boolean = true
      override def nature: Tel.Nature = element0.nature
      override def optional: Boolean = true

      def decoded(telVal: Tel): sequence[element] =
        val builder = Vector.newBuilder[element]

        telVal.subtree.absolve match
          case document: Tel.Document =>
            document.children.bind(_.compounds).each: compound =>
              builder += element0.decoded(Tel.make(compound))

          case compound: Tel.Compound =>
            builder += element0.decoded(telVal)

        builder.result().asInstanceOf[sequence[element]]

  // A `Map` encodes as a sequence of `entries` compounds, each carrying a `key`
  // and a `value` child field. As with other collections the product encoder
  // re-keys the wrapping compound with the field's label.

  given mapEncodable: [key: Tel.Encodable, value: Tel.Encodable]
  =>  Map[key, value] is Tel.Encodable =
    Tel.Encodable(() => Morphology.Dict(key.shape(), value.shape())): map =>
      val entries = Array.from:
        map.stdlib.map: (k, v) =>
          val keyChild   = reKey(key.encoded(k), t"key")
          val valueChild = reKey(value.encoded(v), t"value")
          reKey(Tel.compound(t"", Array.empty, Array.of(keyChild, valueChild)), t"entries")

      Tel.compound(t"", Array.empty, entries)

  given mapDecodable: [key, value]
  =>  ( keyCodec:   key is Tel.Decodable,
        valueCodec: value is Tel.Decodable,
        tactic:     Tactic[Tel.Error] )
  =>  ((Map[key, value] is Tel.Decodable)^{tactic}) =
    Tel.Decodable(() => Morphology.Dict(keyCodec.shape(), valueCodec.shape())): telVal =>
      var accumulator = Map.empty[key, value]

      for entry <- telVal.fields(t"entries") do
        // A missing `key`/`value` child routes through `absent()` rather
        // than decoding an empty node, so flag-natured values report their
        // absent form (`false`) instead of misreading emptiness.
        val k = entry.field(t"key").lay(keyCodec.absent())(keyCodec.decoded(_))
        val v = entry.field(t"value").lay(valueCodec.absent())(valueCodec.decoded(_))
        accumulator = accumulator.updated(k, v)

      accumulator

  // Helpers used by encoders to construct Tel values.

  // The §22.3 atom-form escalation (inline -> source -> literal) keeps every
  // encoded value reparseable — a multi-line or space-edged Text can never be
  // an inline atom. The default `#` sigil is assumed, matching the documents
  // the encoder produces (it never emits a pragma overriding it). An empty
  // text stays an empty inline atom: presentationally it serializes as no
  // atom, but the value level distinguishes present-empty from absent.
  def scalar(text: Text): Tel =
    Tel.make(Tel.Compound(t"", Array.of(Mutation.chooseAtomForm(text, '#')), Unset, Array.empty))

  def compound
    ( keyword: Text, atoms: Array[Tel.Atom]^{}, compounds: Array[Tel.Compound]^{} )
  :   Tel =

    val children =
      if compounds.nil then Array.empty[Tel.Block]
      else Array.of(Tel.Block(Array.empty, Unset, compounds, 0))

    Tel.make(Tel.Compound(keyword, atoms, Unset, children))

  def empty: Tel = Tel.make(Tel.Compound(t"", Array.empty, Unset, Array.empty))

// `value.encode` (provided by the Encodable typeclass extension defined in
// anticipation) is the idiomatic call site producing a Tel from any
// encodable value. A `.tel` alias may be added later for symmetry with
// jacinta's `.json`.
