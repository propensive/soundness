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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import anticipation.*
import vacuous.*

// Validator infrastructure per §21 of the TEL specification.
//
// A validator is a named helper method: given a `ValidationRequest` —
// either a scalar value-text or a struct element — it returns
// `Valid` or `Invalid(diagnostic)`. A schema names validators in the
// `validators` lists of Scalar / Struct / SelectDefinition; the parser
// applies them in AND-conjunction.
//
// Each parser instance is configured with a `ValidatorRegistry` (a
// callback per validator name). When no registry is provided, no
// validation runs and no E310 errors are raised (§21.4).

object TelValidator:

  enum Request:
    case Scalar(method: Text, value: Text)
    case Struct(method: Text, element: TelElement.Node)

  enum Diagnostic:
    case Scalar
       ( message: Text,
         span:    Optional[(Int, Int)] = Unset )

    case Struct
       ( message: Text,
         fields:  Map[Text, Diagnostic] = Map.empty )

  enum Response:
    case Valid
    case Invalid(diagnostic: Diagnostic)

  // The callback type. A `ValidatorRegistry` resolves a validator name
  // and a request to a response. The default registry includes the
  // four built-in validators required by `tel-schema` (§21.5).
  trait Registry:
    def apply(request: Request): Response

  object Registry:
    val builtins: Registry = new Registry:
      override def apply(request: Request): Response = request match
        case Request.Scalar(method, value) => method.s match
          case "string"     => Response.Valid
          case "identifier" => identifier(value)
          case "type-name"  => typeName(value)
          case "sigil"      => sigil(value)
          case _            => unknown(method)

        case Request.Struct(method, _) =>
          // None of the four built-ins support struct requests.
          Response.Invalid(Diagnostic.Struct(
            Text(s"validator '${method.s}' not applicable to struct values")))

    // Compose a custom Registry with the built-ins as fallback.
    def withFallback(custom: Registry): Registry = new Registry:
      override def apply(request: Request): Response =
        custom(request) match
          case Response.Valid                                       => Response.Valid
          case Response.Invalid(d) if isUnknown(d)                  => builtins(request)
          case other                                                => other

    private def isUnknown(d: Diagnostic): Boolean = d match
      case Diagnostic.Scalar(m, _) => m.s.startsWith("unknown validator")
      case _                       => false

    // The four built-in scalar validators of §21.5.

    private def identifier(value: Text): Response =
      val s = value.s
      if s.isEmpty then fail("the identifier must not be empty", (0, 0))
      else if s.startsWith("-") then fail("the identifier must not begin with a hyphen", (0, 1))
      else if s.endsWith("-") then fail("the identifier must not end with a hyphen",
        (s.length - 1, s.length))
      else if s.contains("--") then fail("the identifier must not contain consecutive hyphens",
        (s.indexOf("--"), s.indexOf("--") + 2))
      else
        var i = 0
        while i < s.length do
          val c = s.charAt(i)
          if !(c == '-' || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')) then
            return fail(s"identifier character '$c' must be lowercase ASCII letter, digit, or hyphen",
              (i, i + 1))

          i += 1

        Response.Valid

    private def typeName(value: Text): Response =
      val s = value.s
      if s.isEmpty then fail("the type name must not be empty", (0, 0))
      else
        val first = s.charAt(0)
        if !(first >= 'A' && first <= 'Z') then
          fail("the type name must start with an uppercase ASCII letter", (0, 1))
        else
          var i = 1
          while i < s.length do
            val c = s.charAt(i)
            if !((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) then
              return fail(s"type-name character '$c' must be ASCII alphanumeric", (i, i + 1))

            i += 1

          Response.Valid

    private def sigil(value: Text): Response =
      val s = value.s
      if s.length != 1 then fail("the sigil must be a single character", (0, s.length))
      else
        val c = s.charAt(0)
        if c == ' ' || c == '\n' || c == '\r' || c == '\t' then
          fail("the sigil must not be whitespace", (0, 1))
        else if (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then
          fail("the sigil must not be a letter or digit", (0, 1))
        else if "()[]{}<>".indexOf(c.toInt) >= 0 then
          fail("the sigil must not be a parenthetical symbol", (0, 1))
        else Response.Valid

    private def unknown(method: Text): Response =
      Response.Invalid(Diagnostic.Scalar(Text(s"unknown validator '${method.s}'")))

    private def fail(message: String, span: (Int, Int)): Response =
      Response.Invalid(Diagnostic.Scalar(Text(message), span))
