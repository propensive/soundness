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
package distillate

// The codec path, as a contextual mode. Every format with a generated
// ("direct") codec offers two paths to a typed value: the derived one, which
// builds the format's syntax tree and decodes it (or encodes to one), and the
// direct one, which drives the format's token reader or printer straight into
// or out of the value. A format's companion contributes givens conditional on
// one of these modes, so a single named import selects the path for every
// format at once:
//
//     import codecs.direct     // generated codecs wherever a type is inlinable
//     import codecs.derived    // the derived codecs, even over a direct instance
//     import codecs.strict     // generated codecs, and a compile error where there is none
//
// With none in scope, a type takes the direct path exactly when it has a
// direct instance of its own (an explicit given, or `derives`), and the
// derived path otherwise. Results are the same on either path; only the speed
// and the positions in error diagnostics differ.
//
// `Strict` is a `Direct`, so a format's generated instances apply under both;
// strictness is added at the entry points (`read[Foo in F]`, `in[Text over
// F]`, `in[Data over F]`), which a format gates with `Codecs.strictFailure`
// where no direct instance resolves, so that nothing falls back to the derived
// path silently. The modes are alternatives: importing two at once makes their
// givens ambiguous wherever both would apply.
object Codecs:
  class Direct()
  final class Derived()
  final class Strict() extends Direct()

  // The compile error a format reports under `codecs.strict` for a type with no direct
  // codec: raised after the given it sits in has been selected, so the error is the
  // outcome, not a failed candidate the search could pass over.
  inline def strictFailure[value](inline format: String): Nothing =
    ${internal.strictFailure[value]('format)}

package codecs:
  given direct: Codecs.Direct = Codecs.Direct()
  given derived: Codecs.Derived = Codecs.Derived()
  given strict: Codecs.Strict = Codecs.Strict()
