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
package decorum

// The eight principles from which every Decorum rule derives. Part I of
// doc/standards/syntax.md states each principle and its readability
// motivation; every rule in `Rules.all` cites the principle it derives
// from.
enum Principle:
  case Frame, Anchoring, Density, ContinuationMarking, Balance, Proximity, Tabulation,
    Findability

// One house-style rule: an SN identifier (a family such as `833` may span
// several sub-rules emitted with suffixed identifiers), the principle it
// derives from, and a check over the per-file `Context`. A rule must not
// throw on malformed input: extraction failures surface as an absence of
// data, never as an exception.
trait Rule:
  def id: String
  def principle: Principle
  def check(ctx: Context): List[Violation]

object Rules:
  val all: List[Rule] =
    List
      ( FrameRules.LicenceFrame, FrameRules.PackageDeclaration, FrameRules.PackageBlank,
        FrameRules.ImportSeparation, FrameRules.ImportOrdering,
        AnchorRules.SequenceLayout, AnchorRules.DefinitionAnchors,
        // OperatorContinuation must precede ContinuationIndent: both can fire
        // at the same position (a continuation line led by an operator), and
        // dotty's reporter keeps only the first diagnostic per position — 616
        // is the more specific message there.
        ContinuationRules.OperatorContinuation,
        AnchorRules.BodyScopeIndent, AnchorRules.ContinuationIndent,
        AnchorRules.SignatureEqLast,
        AnchorRules.InterpolationLayout, TabulationRules.CaseAlignment,
        TabulationRules.ForComprehensionAlignment, DensityRules.LambdaLayout,
        ProximityRules.ChunkSeparation,
        FindabilityRules.FileNaming, FindabilityRules.CompanionOrdering,
        FindabilityRules.SoundnessExportCompleteness,
        FindabilityRules.ExtensionExportCompleteness,
        // LineLength (230) fired at the top of the old per-line walk, before
        // the quote/splice family — it must keep winning their one positional
        // collision (a 101-column line inside an inline splice).
        FrameRules.LineLength,
        // These four fired towards the end of the old per-line walk (in this
        // order: 444, 163, 140, 677), after LineLength and before the
        // quote/splice family, so they sit between those two here. No
        // positional collision with any other rule exists in the corpus, but
        // the relative order is preserved regardless.
        ContinuationRules.HardSpace, ContinuationRules.ChainContinuation,
        AnchorRules.GivenArrowAlign, ProximityRules.ReturnTypeBlank,
        // QuoteSpliceLayout stays last: the 473.2–.7 family used to fire at
        // the end of the per-line walk, after every registry rule, so any
        // positional collision with an earlier rule (e.g. 616.1 with 473.5)
        // must keep resolving in the earlier rule's favour — dotty's
        // reporter keeps only the first diagnostic per position.
        QuoteRules.QuoteSpliceLayout )
