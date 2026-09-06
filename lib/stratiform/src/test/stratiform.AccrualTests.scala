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

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import charEncoders.utf8Encoder
import denominative.dysasymptotics.linearSize

case class APerson(name: Text, age: Int, email: Text) derives CanEqual
case class AContact(person: APerson, company: Text) derives CanEqual
case class APair(width: Int, height: Int) derives CanEqual

object TProbe:
  @scala.caps.unsafe.untrackedCaptures
  var constructions: Int = 0

// The body statement observes construction: decoding must never construct from garbage
// fallback values, so a decode with any failed field must leave the counter untouched.
case class TChecked(name: Text, age: Int) derives CanEqual:
  TProbe.constructions += 1

object AccrualTests extends Suite(m"Stratiform multi-error accrual tests"):

  case class Issues(items: List[(Text, Tel.Error)] = Nil)(using Diagnostics)
  extends Error(m"${items.size} decoding issues"):
    def +(focus: Text, error: Tel.Error): Issues = Issues(items :+ (focus, error))

  // Inline, with a directly-constructed `Validate`: a `raises … tracks …` function VALUE
  // cannot be typed under capture checking (its honest type is a curried dependent context
  // function, an unimplemented compiler restriction), so the decode lambda must beta-reduce
  // away into `protect`'s inline position. See rep/DECISIONS.md.
  private inline def validateTel[result](tel: Tel)
                                 (inline decode: Tel => result raises Tel.Error tracks Tel.Focus)
  :   Issues =
    Validate[Issues, [r] =>> r raises Tel.Error, Tel.Focus]
      ( Issues(),
        { case error: Tel.Error =>
            accrual + (prior.let(_.pointer.encode).or(t"/"), error) } )
    . protect(decode(tel))

  private def validateAssign(tel: Tel, schema: Tels): Issues =
    validate[Tel.Focus](Issues()):
      case error: Tel.Error =>
        accrual + (prior.let(_.pointer.encode).or(t"/"), error)
    . protect(Tel.Type.assign(tel, schema))

  case class Located(items: List[(Text, Span)] = Nil)(using Diagnostics)
  extends Error(m"${items.size} located issues"):
    def +(pointer: Text, span: Span): Located = Located(items :+ (pointer, span))

  // Validate a *tracked* document against `schema`, capturing each error's
  // keyword-path pointer alongside the source `Span` filled in by
  // `Tel.supplementPositions` at the end of `Tel.Type.assign`.
  private def assignPositions(text: Text, schema: Tels): Located =
    import parsing.trackPositions
    val tel = text.read[Tel]

    validate[Tel.Focus](Located()):
      case error: Tel.Error =>
        accrual + (prior.let(_.pointer.encode).or(t"/"), prior.lay(Span.empty)(_.span))
    . protect(Tel.Type.assign(tel, schema))

  // The decode-path counterpart: `Tel#as` locates its per-field foci against
  // the same tracked root. Inline for the same reason as `validateTel`.
  private inline def decodePositions[result](text: Text)
                                    (inline decode: Tel => result raises Tel.Error tracks Tel.Focus)
  :   Located =
    import parsing.trackPositions
    val tel = text.read[Tel]

    Validate[Located, [r] =>> r raises Tel.Error, Tel.Focus]
      ( Located(),
        { case error: Tel.Error =>
            accrual + (prior.let(_.pointer.encode).or(t"/"),
                       prior.lay(Span.empty)(_.span)) } )
    . protect(decode(tel))

  // The direct-path counterpart of `decodePositions`: `read[APerson in Tel]`
  // resolves to `aggregableParsed` because a `Tel.Parsable` is in scope, so no
  // AST is built and every span must have been stamped during the parse.
  //
  // Two things about the shape of these, both load-bearing. The `Tel.Parsable`
  // is derived *inside* `protect`, because `Tel.Parsable.product` takes its
  // `Foci` and `Tactic` when the instance is constructed, not when it parses —
  // an instance built at object level closes over the inert default `Foci` and
  // the file-level `throwUnsafely`, and would neither accrue nor be located.
  // (Its type says so: a `Tel.Parsable` is capture-tracked over its tactic.)
  // And they are written out per type rather than as one generic helper, since
  // a second layer of `inline` indirection hoists the same summons back out of
  // the boundary — the reason `validateTel` above exists.
  private def directPerson(text: Text): Located =
    import parsing.trackPositions

    Validate[Located, [r] =>> r raises Tel.Error, Tel.Focus]
      ( Located(),
        { case error: Tel.Error =>
            accrual + (prior.let(_.pointer.encode).or(t"/"),
                       prior.lay(Span.empty)(_.span)) } )
    . protect:
        given APerson is Tel.Parsable = Tel.Parsable.derived
        text.read[APerson in Tel]

  private def directContact(text: Text): Located =
    import parsing.trackPositions

    Validate[Located, [r] =>> r raises Tel.Error, Tel.Focus]
      ( Located(),
        { case error: Tel.Error =>
            accrual + (prior.let(_.pointer.encode).or(t"/"),
                       prior.lay(Span.empty)(_.span)) } )
    . protect:
        given APerson is Tel.Parsable = Tel.Parsable.derived
        given AContact is Tel.Parsable = Tel.Parsable.derived
        text.read[AContact in Tel]

  // Parse a document under an accrual boundary: recoverable parse defects
  // (§19.5) accrue rather than aborting on the first, because `read[Tel]` parses
  // through the installed `TrackTactic`.
  private def validateRead(text: Text): Issues =
    validate[Tel.Focus](Issues()):
      case error: Tel.Error =>
        accrual + (prior.let(_.pointer.encode).or(t"/"), error)
    . protect(text.read[Tel])

  // Negative-corpus fixtures whose accrued diagnostics legitimately differ
  // from the reference parser's report, each with the reason recorded here.
  // Keep this list small: every entry weakens the no-silent-truncation
  // guarantee below for that fixture.
  private val strictExemptions: List[Text] = List
    ( t"e119-non-space-after-marker" ) // we report per heading; reference reports per marker

  // A document schema with two required scalar fields and no defaults: a document
  // omitting both yields two `RequiredMemberAbsent` violations.
  private val twoRequiredSchema: Tels = Tels(
    name     = t"pair",
    document = Tels.Struct(
      members = Array(
        Tels.Field
         ( Tels.Polarity.Implicit, Tels.Polarity.Implicit,
           t"name", Tels.Scalar(Array(t"string")), Unset ),
        Tels.Field
         ( Tels.Polarity.Implicit, Tels.Polarity.Implicit,
           t"email", Tels.Scalar(Array(t"string")), Unset )),
      validators = Array.empty),
    layers   = Array.empty,
    sigil    = Unset,
    records  = Array.empty,
    scalars  = Array.empty,
    selects  = Array.empty)

  // A document schema with a single optional field: unrecognised keywords yield
  // `UnknownKeyword` violations without any required-member errors.
  private val optionalFieldSchema: Tels = Tels(
    name     = t"loose",
    document = Tels.Struct(
      members = Array(
        Tels.Field
         ( Tels.Polarity.Loose, Tels.Polarity.Implicit,
           t"name", Tels.Scalar(Array(t"string")), Unset )),
      validators = Array.empty),
    layers   = Array.empty,
    sigil    = Unset,
    records  = Array.empty,
    scalars  = Array.empty,
    selects  = Array.empty)

  // An optional `item` record member (one required scalar) beside a required
  // `name` scalar: an atom-phase defect inside `item` must accrue alongside
  // the missing-member defect at the root rather than aborting.
  private val atomAccrualSchema: Tels = Tels(
    name     = t"atoms",
    document = Tels.Struct(
      members = Array(
        Tels.Field
         ( Tels.Polarity.Loose, Tels.Polarity.Implicit,
           t"item", Tels.Reference(t"Item"), Unset ),
        Tels.Field
         ( Tels.Polarity.Implicit, Tels.Polarity.Implicit,
           t"name", Tels.Scalar(Array(t"string")), Unset )),
      validators = Array.empty),
    layers   = Array.empty,
    sigil    = Unset,
    records  = Array(Tels.RecordDefinition(
      t"Item",
      Array(Tels.Field
       ( Tels.Polarity.Implicit, Tels.Polarity.Implicit,
         t"only", Tels.Scalar(Array(t"string")), Unset )),
      Array.empty)),
    scalars  = Array.empty,
    selects  = Array.empty)

  // Like `atomAccrualSchema`, but `Item` carries a single required Flag: a
  // mismatched atom is E305 and the flag then also reports absent (E307).
  private val flagAccrualSchema: Tels = Tels(
    name     = t"flags",
    document = Tels.Struct(
      members = Array(
        Tels.Field
         ( Tels.Polarity.Loose, Tels.Polarity.Implicit,
           t"item", Tels.Reference(t"Item"), Unset ),
        Tels.Field
         ( Tels.Polarity.Implicit, Tels.Polarity.Implicit,
           t"name", Tels.Scalar(Array(t"string")), Unset )),
      validators = Array.empty),
    layers   = Array.empty,
    sigil    = Unset,
    records  = Array(Tels.RecordDefinition(
      t"Item",
      Array(Tels.Field(Tels.Polarity.Implicit, Tels.Polarity.Implicit, t"a", Tels.Flag, Unset)),
      Array.empty)),
    scalars  = Array.empty,
    selects  = Array.empty)

  def run(): Unit =
    suite(m"Single-error decoding (sanity)"):
      test(m"Fully-valid record: no errors accrued"):
        val tel = t"name Alice\nage 30\nemail a@b.c\n".read[Tel]
        validateTel(tel)(_.as[APerson]).items.size
      . assert(_ == 0)

      test(m"Single missing field: one error"):
        val tel = t"name Alice\nage 30\n".read[Tel]
        validateTel(tel)(_.as[APerson]).items.size
      . assert(_ == 1)

      test(m"Single wrong-type field: one error"):
        val tel = t"width five\nheight 10\n".read[Tel]
        validateTel(tel)(_.as[APair]).items.size
      . assert(_ == 1)

    suite(m"Gated construction"):
      test(m"Constructor does not run when any field failed"):
        TProbe.constructions = 0
        val tel = t"name Zoe\nage young\n".read[Tel]
        val issues = validateTel(tel)(_.as[TChecked])
        (issues.items.size, TProbe.constructions)
      . assert(_ == (1, 0))

      test(m"Constructor runs exactly once when all fields are clean"):
        TProbe.constructions = 0
        val tel = t"name Zoe\nage 5\n".read[Tel]
        validateTel(tel)(_.as[TChecked])
        TProbe.constructions
      . assert(_ == 1)

    suite(m"Multiple missing fields"):
      test(m"Two missing fields accrue two errors"):
        val tel = t"name Alice\n".read[Tel]
        validateTel(tel)(_.as[APerson]).items.size
      . assert(_ == 2)

      test(m"Pointers identify the missing fields"):
        val tel = t"name Alice\n".read[Tel]
        validateTel(tel)(_.as[APerson]).items.map(_(0).s).to[Set]
      . assert(_ == Set("/age", "/email"))

      test(m"Each missing-field error has reason Absent"):
        val tel = t"name Alice\n".read[Tel]
        validateTel(tel)(_.as[APerson]).items.all:
          case (_, err) => err.reason == Tel.Error.Reason.Absent
      . assert(identity)

    suite(m"Multiple wrong-type fields"):
      test(m"Two wrong types accrue two errors"):
        val tel = t"width wide\nheight tall\n".read[Tel]
        validateTel(tel)(_.as[APair]).items.size
      . assert(_ == 2)

      test(m"Pointers identify the wrong-type fields"):
        val tel = t"width wide\nheight tall\n".read[Tel]
        validateTel(tel)(_.as[APair]).items.map(_(0).s).to[Set]
      . assert(_ == Set("/width", "/height"))

      test(m"Wrong-type errors have reason NotScalar"):
        val tel = t"width wide\nheight tall\n".read[Tel]
        validateTel(tel)(_.as[APair]).items.all:
          case (_, err) => err.reason match
            case Tel.Error.Reason.NotScalar(_, _) => true
            case _                               => false
      . assert(identity)

    suite(m"Nested case-class errors"):
      test(m"Missing nested case-class field expands per sub-field"):
        val tel = t"company Acme\n".read[Tel]
        validateTel(tel)(_.as[AContact]).items.map(_(0).s).to[Set]
      . assert: paths =>
          paths == Set
           ( "/person/name",
             "/person/age",
             "/person/email" )

    suite(m"Regression: does not abort on the first bad field"):
      test(m"Both wrong-type fields are reported, not just the first"):
        val tel = t"width wide\nheight tall\n".read[Tel]
        validateTel(tel)(_.as[APair]).items.size
      . assert(_ > 1)

    suite(m"Schema-validation accrual (E3xx)"):
      test(m"Two missing required members accrue two errors"):
        val doc = t"".read[Tel]
        validateAssign(doc, twoRequiredSchema).items.size
      . assert(_ == 2)

      test(m"Both missing-member errors have reason RequiredMemberAbsent"):
        val doc = t"".read[Tel]
        validateAssign(doc, twoRequiredSchema).items.all:
          case (_, err) => err.reason == Tel.Error.Reason.RequiredMemberAbsent
      . assert(identity)

      test(m"Two unknown keywords accrue two errors"):
        val doc = t"foo a\nbar b\n".read[Tel]
        validateAssign(doc, optionalFieldSchema).items.size
      . assert(_ == 2)

      test(m"Both unknown-keyword errors have reason UnknownKeyword"):
        val doc = t"foo a\nbar b\n".read[Tel]
        validateAssign(doc, optionalFieldSchema).items.all:
          case (_, err) => err.reason == Tel.Error.Reason.UnknownKeyword
      . assert(identity)

    suite(m"Type-assignment accrual (atom phase and constraints)"):
      test(m"An excess atom and a missing member accrue together"):
        val doc = t"item x y\n".read[Tel]
        validateAssign(doc, atomAccrualSchema).items.map(_(1).reason).to[Set]
      . assert(_ == Set(Tel.Error.Reason.TooManyAtoms, Tel.Error.Reason.RequiredMemberAbsent))

      test(m"A mismatched flag atom accrues E305 and the flag reports absent"):
        val doc = t"item xyz\nname n\n".read[Tel]
        validateAssign(doc, flagAccrualSchema).items.map(_(1).reason).to[Set]
      . assert: reasons =>
          reasons == Set
           ( Tel.Error.Reason.AtomFlagKeywordMismatch,
             Tel.Error.Reason.RequiredMemberAbsent )

      test(m"A duplicated non-repeatable member accrues a single E308"):
        val doc = t"name Alice\nname Bob\nemail e\n".read[Tel]
        validateAssign(doc, twoRequiredSchema).items.map(_(1).reason).to[Set]
      . assert(_ == Set(Tel.Error.Reason.NonRepeatableTooMany))

    suite(m"Parser-recovery accrual (E1xx)"):
      test(m"Two trailing-space lines accrue two errors"):
        validateRead(t"good \nbad \n").items.size
      . assert(_ == 2)

      test(m"Both are TrailingSpaces errors"):
        validateRead(t"good \nbad \n").items.all:
          case (_, err) => err.reason == Tel.Error.Reason.TrailingSpaces
      . assert(identity)

      test(m"A single recoverable defect still accrues one error"):
        validateRead(t"good \nfine\n").items.size
      . assert(_ == 1)

      test(m"A malformed pragma version and a trailing-space line accrue together"):
        validateRead(t"tel bad\ngood \n").items.size
      . assert(_ == 2)

      test(m"The accrued reasons span the pragma and the body"):
        validateRead(t"tel bad\ngood \n").items.map(_(1).reason).to[Set]
      . assert(_ == Set(Tel.Error.Reason.BadVersion, Tel.Error.Reason.TrailingSpaces))

      test(m"A bad schema identifier recovers and the body still accrues"):
        validateRead(t"tel 1.0 bad!id\ngood \n").items.map(_(1).reason).to[Set]
      . assert(_ == Set(Tel.Error.Reason.BadPragmaPhrase, Tel.Error.Reason.TrailingSpaces))

      test(m"Two odd-indented lines accrue two OddIndentation errors"):
        validateRead(t"a\n b\n c\n").items.stdlib.map(_(1).reason).to(List)
      . assert(_ == List(Tel.Error.Reason.OddIndentation, Tel.Error.Reason.OddIndentation))

      test(m"An over-indented line recovers and a later defect still accrues"):
        validateRead(t"parent\n        too-deep\nc \n").items.map(_(1).reason).to[Set]
      . assert(_ == Set(Tel.Error.Reason.OverIndentation, Tel.Error.Reason.TrailingSpaces))

      test(m"Many defects across a document all accrue (LSP scenario)"):
        validateRead(t"tel bad\nparent\n        too-deep\ntail \n").items.map(_(1).reason).to[Set]
      . assert: reasons =>
          reasons == Set
           ( Tel.Error.Reason.BadVersion,
             Tel.Error.Reason.OverIndentation,
             Tel.Error.Reason.TrailingSpaces )

      // Issue #1834: a blank line followed by a deeper line used to unwind the
      // whole parse, silently dropping the rest of the document — the AST and
      // every subsequent diagnostic.
      test(m"A blank line before a deeper child is not itself an error (§9)"):
        validateRead(t"parent\n\n  child\n").items.size
      . assert(_ == 0)

      test(m"A blank line before a deeper comment block is valid (§11.1)"):
        validateRead(t"parent\n\n  # note\n  child\n").items.size
      . assert(_ == 0)

      test(m"A defect after a blank-then-deeper line still accrues (#1834)"):
        validateRead(t"tel 1.0\n\nparent\n\n  child\n   bogus\n").items.stdlib.map(_(1).reason).to(List)
      . assert(_ == List(Tel.Error.Reason.OddIndentation))

      test(m"The post-blank odd-indent defect is located on its own line"):
        validateRead(t"tel 1.0\n\nparent\n\n  child\n   bogus\n").items.stdlib.map(_(1).span.startLine).to(List)
      . assert(_ == List(5.z))

      test(m"Blank-then-over-indented recovers and later defects accrue"):
        validateRead(t"parent\n\n    deep\ntail \n").items.map(_(1).reason).to[Set]
      . assert(_ == Set(Tel.Error.Reason.OverIndentation, Tel.Error.Reason.TrailingSpaces))

      test(m"Blank-then-deeper after a tabulation header blames the tabulation"):
        validateRead(t"# a  # b\n\n    deep\n").items.map(_(1).reason).to[Set]
      . assert(_.has(Tel.Error.Reason.RowWrongIndent))

      test(m"A misplaced phrase and a formless phrase accrue separately"):
        validateRead(t"tel 1.0 example.com/a example.com/b nonsense\nhello\n")
        . items.map(_(1).reason).to[Set]
      . assert(_ == Set(Tel.Error.Reason.MisplacedPragmaPhrase, Tel.Error.Reason.BadPragmaPhrase))

    suite(m"Corpus diagnostics are complete (no silent truncation)"):
      // The corpus membership checks in stratiform_test pass if any ONE
      // expected code is raised, so a parser that reports the first defect and
      // silently stops (as blank-then-deeper once made it — issue #1834) still
      // passes them. These compare the accrued multiset of codes against the
      // reference parser's full report from the `.check` file, and require the
      // whole positive corpus to accrue nothing.
      CorpusLoader.negative.each: testcase =>
        val expected = CheckFormat.parse(testcase.check).errors.map(_.code)

        if expected.stdlib.nonEmpty && expected.stdlib.forall(_ < 200)
        && !strictExemptions.stdlib.contains(testcase.stem)
        then
          test(m"accrues exactly the reference diagnostics on ${testcase.stem}"):
            validateRead(testcase.source.utf8).items.stdlib.map(_(1).reason.number).sorted
          . assert(_ == expected.stdlib.sorted)

      CorpusLoader.positive.each: testcase =>
        test(m"accrues no diagnostics on ${testcase.stem}"):
          validateRead(testcase.source.utf8).items.size
        . assert(_ == 0)

    suite(m"Located schema-validation errors (LSP diagnostics)"):
      test(m"Unknown-keyword errors carry their keyword pointer"):
        assignPositions(t"foo a\nbar b\n", optionalFieldSchema).items.map(_(0).s).to[Set]
      . assert(_ == Set("/foo", "/bar"))

      test(m"Unknown-keyword errors are located at the offending compound"):
        assignPositions(t"foo a\nbar b\n", optionalFieldSchema).items.map(_(1)).to[Set]
      . assert(_ == Set(Tel.Error.spanAt(1, 1, 3), Tel.Error.spanAt(2, 1, 3)))

      test(m"An unlocated (untracked) validation still accrues without a position"):
        val tel = t"foo a\n".read[Tel]

        validate[Tel.Focus](Issues()):
          case error: Tel.Error =>
            accrual + (prior.lay(t"")(f => if f.span.vacant then t"" else Tel.Error.describe(f.span)),
                       error)
        . protect(Tel.Type.assign(tel, optionalFieldSchema))
        . items.map(_(0).s).to[Set]
      . assert(_ == Set(""))

      test(m"Missing required members carry a pointer but no source span"):
        assignPositions(t"", twoRequiredSchema).items.map { case (p, span) => (p.s, span.exists) }
        . to[Set]
      . assert(_ == Set(("/name", false), ("/email", false)))

      // The excess atom is read by `assignAtoms`, deep inside `assignCompound`,
      // so this proves the per-compound focus covers a compound's whole subtree
      // and not just the keyword-dispatch step — the issue's `e302.tel` case.
      test(m"An excess atom is located at the enclosing compound"):
        assignPositions(t"item x y\nname n\n", atomAccrualSchema).items
        . map { case (pointer, span) => (pointer.s, span.startLine.lay(-1)(_.n1)) }.to[Set]
      . assert(_ == Set(("/item", 1)))

      // E308 is a property of a member's whole run, not of one node, so it is
      // raised outside every `focus` block: the entry has no focus at all and
      // takes the root one. `supplementPositions` must tolerate that rather
      // than assume it is present.
      test(m"A run-level E308 accrues at the root without panicking"):
        assignPositions(t"name Alice\nname Bob\nemail e\n", twoRequiredSchema).items
        . map { case (p, span) => (p.s, span.exists) }.to[Set]
      . assert(_ == Set(("/", false)))

    suite(m"Located decode errors"):
      test(m"A malformed field's focus names the field"):
        decodePositions(t"name Alice\nage notanumber\nemail e\n")(_.as[APerson])
        . items.map(_(0).s).to[Set]
      . assert(_ == Set("/age"))

      test(m"A malformed field is located at its value, not its keyword"):
        decodePositions(t"name Alice\nage notanumber\nemail e\n")(_.as[APerson])
        . items.map(_(1)).to[Set]
      . assert(_ == Set(Tel.Error.spanAt(2, 5, 10)))

    // A value with any fallible scalar field can only be read through the direct
    // path, so the spans it accrues have to be as good as the AST path's. There
    // is no document to locate against once a direct parse ends, so the spans are
    // stamped as it goes; these tests are what pin the two answers together.
    suite(m"Located decode errors (direct path)"):
      test(m"The direct path accrues a located focus at all"):
        directPerson(t"name Alice\nage notanumber\nemail e\n")
        . items.map { case (pointer, span) => (pointer.s, span.exists) }.to[Set]
      . assert(_ == Set(("/age", true)))

      test(m"A malformed field is located at its value, as on the AST path"):
        directPerson(t"name Alice\nage notanumber\nemail e\n").items.map(_(1)).to[Set]
      . assert(_ == Set(Tel.Error.spanAt(2, 5, 10)))

      // The acceptance criterion for issue #1726: same document, both decode
      // paths, identical (pointer, span) pairs. `PositionalTests`' own parity
      // helper compares decoded values only, which is why this gap went unseen.
      test(m"Both paths agree on a malformed leaf field's focus"):
        val doc = t"name Alice\nage notanumber\nemail e\n"
        directPerson(doc).items == decodePositions(doc)(_.as[APerson]).items
      . assert(_ == true)

      test(m"Both paths agree on a malformed field nested in a record"):
        val doc = t"person\n  name Alice\n  age nope\n  email e\ncompany Acme\n"
        directContact(doc).items == decodePositions(doc)(_.as[AContact]).items
      . assert(_ == true)

      test(m"Both paths agree on a missing required field"):
        val doc = t"name Alice\nemail e\n"
        directPerson(doc).items == decodePositions(doc)(_.as[APerson]).items
      . assert(_ == true)

      test(m"A nested field is located at its own value, not its parent's keyword"):
        directContact(t"person\n  name Alice\n  age nope\n  email e\ncompany Acme\n")
        . items.map { case (pointer, span) => (pointer.s, span) }.to[Set]
      . assert(_ == Set(("/person/age", Tel.Error.spanAt(3, 7, 4))))

      // With no `Foci` at all, the focus machinery is inert and the span has to
      // ride on the error itself — the common fail-fast read, and the reason
      // this path is gated on `parsing.trackPositions` rather than on whether
      // errors are being accrued.
      test(m"A fail-fast direct read carries the span on the error itself"):
        import parsing.trackPositions
        given APerson is Tel.Parsable = Tel.Parsable.derived
        capture[Tel.Error](t"name Alice\nage notanumber\nemail e\n".read[APerson in Tel]).span
      . assert(_ == Tel.Error.spanAt(2, 5, 10))
