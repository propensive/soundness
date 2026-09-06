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

import scala.sys

import scala.language.unsafeNulls

import anticipation.*
import gossamer.*
import rudiments.*
import vacuous.*

// Loads upstream TEL test corpus from classpath resources. The corpus is
// laid out under /stratiform/corpus/{pos,neg}/ with paired <stem>.tel input
// files and <stem>.check fixture files. The pos.index / neg.index files
// list one stem per line; tests iterate the index to enumerate cases.

object CorpusLoader:

  case class Case(stem: Text, source: Data, check: Text)

  def positive: List[Case] = load(t"pos")
  def negative: List[Case] = load(t"neg")

  // Multi-document stream fixtures (§6.1): `.check` files hold a sequence of
  // `=== document N ===` sections (see CheckFormat.parseStream).
  def streaming: List[Case] = load(t"stream")

  private def load(category: Text): List[Case] =
    readIndex(category).filter(!_.s.startsWith("_")).map(caseByStem(category, _))

  // Load a single corpus case by category and stem (e.g. an off-index fixture).
  def caseByStem(category: Text, stem: Text): Case =
    val source = readResource(t"/stratiform/corpus/$category/$stem.tel")
    val check = readResourceText(t"/stratiform/corpus/$category/$stem.check")
    Case(stem, source, check)

  // Extract the expected E-code from a negative case's stem name. Filenames
  // follow the upstream convention `e<n>-<description>.tel`; cases without
  // an E-code prefix (`_contact-schema.tel`, `contact-document-missing-name`)
  // return Unset and are skipped by the error-code assertion.
  def expectedCode(stem: Text): Optional[Int] =
    val s = stem.s
    if s.startsWith("e") && s.length > 1 && Character.isDigit(s.charAt(1)) then
      val end =
        var idx = 1
        while idx < s.length && Character.isDigit(s.charAt(idx)) do idx += 1
        idx

      try s.substring(1, end).toInt: Optional[Int] catch case _: NumberFormatException => Unset
    else Unset

  // Acceptable error codes for a negative case. We accept either the
  // filename code (the specific scenario the fixture demonstrates) or
  // any code from the `.check` file's `errors:` section (the reference
  // parser's report). Both are valid — fixtures sometimes describe a
  // condition the reference parser reports under a more general code,
  // and conversely the reference sometimes reports a more general code
  // where a more specific one is equally valid.
  def expectedCodes(testcase: Case): List[Int] =
    val fromCheck = CheckFormat.parse(testcase.check).errors.map(_.code)
    val fromName = expectedCode(testcase.stem).lay(List.empty[Int])(List(_))
    (fromName.stdlib ::: fromCheck.stdlib).to(List).distinct

  // The tail segment of a corpus pragma's schema URL, mirroring the Rust
  // The fixture convention for schema references: the built-in `tels`
  // meta-schema's coordinate (pinned or bare) selects the built-in
  // axiom; any other reference maps its final module-name segment onto
  // a sibling `<segment>.tel` (or auxiliary `_<segment>.tel`) schema
  // document in the same category directory. A signature-only pragma
  // has no stem.
  def referenceStem(pragma: Tel.Pragma): Optional[Text] =
    pragma.reference.let: reference =>
      if reference.isTels then t"tels"
      else Text(reference.name.s.split("[/.]").nn.last.nn)

  // An auxiliary schema document for a corpus case, by reference-stem
  // name: `<name>.tel` in the category directory, falling back to the
  // underscore-prefixed form (`_`-stems are excluded from case
  // enumeration but exist exactly to serve as schemas for sibling
  // fixtures, e.g. `_keyed-schema.tel`).
  def auxiliarySchema(category: Text, name: Text): Optional[Data] =
    def attempt(stem: Text): Optional[Data] =
      val stream = getClass.getResourceAsStream(t"/stratiform/corpus/$category/$stem.tel".s)
      if stream == null then Unset
      else try Array.unsafeFrozen(stream.readAllBytes().nn) finally stream.close()

    attempt(name).or(attempt(t"_$name"))

  private def readIndex(category: Text): List[Text] =
    val text = readResourceText(t"/stratiform/corpus/$category.index")
    text.cut(t"\n").map(_.trim).filter(_ != t"")

  private def readResource(path: Text): Data =
    val stream = getClass.getResourceAsStream(path.s)
    if stream == null then sys.error(s"missing resource: $path")
    try Array.unsafeFrozen(stream.readAllBytes().nn) finally stream.close()

  private def readResourceText(path: Text): Text = readResource(path).utf8
