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
import gossamer.*
import stratiform.*
import vacuous.*

// Scalar validators for the `lira` schema family, composed over the TEL built-ins. Each grammar
// here is normative for the LIRA specification:
//
//  - `base-256-hash`:  exactly 32 characters, each a member of the BASE-256 alphabet
//  - `module-name`:    kebab-case segments joined by `/` or `.` (e.g. `soundness.dev/gossamer`)
//  - `namespace`:      dotted package-style segments (letters, digits, `_`; no leading digit)
//  - `semver`:         exactly `major.minor.patch`, each a natural; no prerelease/build suffixes
//  - `natural`:        a decimal natural with no superfluous leading zero
//  - `discipline-id`:  `<kebab-name>/<positive integer>`, e.g. `tasty/1`
//  - `profile-id`:     the same grammar as a discipline (§11.6), e.g. `jvm/1`
//  - `guarantee`:      `linkage` or `recompilation` (§11.5; behavior is not certifiable)
//  - `tree-path`:      relative `/`-separated path; no empty, `.` or `..` segments
//  - `atom-class`:     `rigid` or `replaceable`
object LiraValidators:
  import Tel.Validator.{Diagnostic, Registry, Request, Response}

  val registry: Registry = Registry.withFallback:
    new Registry:
      override def apply(request: Request): Response = request match
        case Request.Scalar(method, value) => method.s match
          case "base-256-hash" => base256Hash(value)
          case "module-name"   => moduleName(value)
          case "namespace"     => namespace(value)
          case "semver"        => semver(value)
          case "natural"       => natural(value)
          case "discipline-id" => disciplineId(value)
          case "profile-id"    => profileId(value)
          case "guarantee"     => guarantee(value)
          case "tree-path"     => treePath(value)
          case "atom-class"    => atomClass(value)
          case "tag-name"      => tagName(value)
          case _               => unknown(method)

        case Request.Struct(method, _) => unknown(method)

  private def unknown(method: Text): Response =
    Response.Invalid(Diagnostic.Scalar(t"unknown validator '${method}'"))

  private def fail(message: Text, span: (Int, Int)): Response =
    Response.Invalid(Diagnostic.Scalar(message, span))

  private def base256Hash(value: Text): Response =
    if value.s.length != LiraHash.size
    then fail(t"a hash must be exactly ${LiraHash.size} BASE-256 characters", (0, value.s.length))
    else safely(Base256.decodeStrict(value)) match
      case Unset => fail(t"the hash contains characters outside the BASE-256 alphabet", (0, 32))
      case _     => Response.Valid

  private def kebabChar(c: Char): Boolean =
    c == '-' || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

  // One kebab-case segment: lowercase ASCII letters and digits, single hyphens within.
  private def kebab(s: String): Boolean =
    val shape = !s.isEmpty && !s.startsWith("-") && !s.endsWith("-") && !s.contains("--")
    shape && s.forall(kebabChar)

  private def moduleName(value: Text): Response =
    val s = value.s
    def good(part: String | Null): Boolean = kebab(part.nn)

    if s.isEmpty then fail(t"the module name must not be empty", (0, 0))
    else if !s.split("[/.]", -1).nn.forall(good)
    then fail(t"each `/`- or `.`-separated segment must be kebab-case", (0, s.length))
    else Response.Valid

  // A tag name (§12.6): a letter followed by letters, digits, `-` and `.` — `jdk-19`,
  // `scala-3.9`.
  private def tagName(value: Text): Response =
    val s = value.s

    def tagChar(c: Char): Boolean =
      c == '-' || c == '.' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')

    if s.isEmpty then fail(t"the tag must not be empty", (0, 0))
    else if !s.charAt(0).isLetter
    then fail(t"a tag must start with a letter", (0, 1))
    else if !s.forall(tagChar)
    then fail(t"a tag may contain only letters, digits, `-` and `.`", (0, s.length))
    else Response.Valid

  private def namespaceChar(c: Char): Boolean =
    c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

  private def namespace(value: Text): Response =
    val s = value.s

    def segment(part: String | Null): Boolean =
      val text = part.nn
      val leading = !text.isEmpty && !(text.charAt(0) >= '0' && text.charAt(0) <= '9')
      leading && text.forall(namespaceChar)

    if s.isEmpty then fail(t"the namespace must not be empty", (0, 0))
    else if !s.split("\\.", -1).nn.forall(segment)
    then fail(t"each dotted segment must be a package-style identifier", (0, s.length))
    else Response.Valid

  private def digit(c: Char): Boolean = c >= '0' && c <= '9'

  private def naturalNumber(s: String | Null): Boolean =
    val text = s.nn
    !text.isEmpty && text.forall(digit) && (text == "0" || !text.startsWith("0"))

  private def natural(value: Text): Response =
    if naturalNumber(value.s) then Response.Valid
    else fail(t"a natural number with no leading zero is required", (0, value.s.length))

  private def semver(value: Text): Response =
    val parts = value.s.split("\\.", -1).nn

    if parts.length != 3 || !parts.forall(naturalNumber)
    then fail(t"the version must be `major.minor.patch`, each a natural", (0, value.s.length))
    else Response.Valid

  private def disciplineId(value: Text): Response =
    val parts = value.s.split("/", -1).nn

    if parts.length != 2 || !kebab(parts(0).nn) || !naturalNumber(parts(1)) || parts(1) == "0"
    then fail(t"a discipline is identified as `<name>/<positive integer>`", (0, value.s.length))
    else Response.Valid

  // §11.6: a profile is identified on the same terms as a discipline, and must likewise bump its
  // version on any change to a predicate.
  private def profileId(value: Text): Response =
    val parts = value.s.split("/", -1).nn

    if parts.length != 2 || !kebab(parts(0).nn) || !naturalNumber(parts(1)) || parts(1) == "0"
    then fail(t"a profile is identified as `<name>/<positive integer>`", (0, value.s.length))
    else Response.Valid

  // §11.5 names three guarantee levels, but only two can be claimed or broken: behavior is not
  // certified by any hash scheme, so it is not expressible in a `breaks` field.
  private def guarantee(value: Text): Response =
    if value.s == "linkage" || value.s == "recompilation" then Response.Valid
    else fail(t"a guarantee level is `linkage` or `recompilation`", (0, value.s.length))

  private def treePath(value: Text): Response =
    val s = value.s
    def segment(part: String | Null): Boolean = !part.nn.isEmpty && part != "." && part != ".."

    if s.isEmpty then fail(t"the path must not be empty", (0, 0))
    else if !s.split("/", -1).nn.forall(segment)
    then fail(t"the path must be relative, with no empty, `.` or `..` segments", (0, s.length))
    else Response.Valid

  private def atomClass(value: Text): Response =
    if value.s == "rigid" || value.s == "replaceable" then Response.Valid
    else fail(t"the atom class must be `rigid` or `replaceable`", (0, value.s.length))
