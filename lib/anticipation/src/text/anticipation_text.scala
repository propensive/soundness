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
package anticipation

extension (texts: Iterable[Text])
  transparent inline def ss: Iterable[String] = texts.map(_.s)

extension (string: String) def tt: Text = Text(string)

// Churn-reduction shim: a converted literal flowing into an existing
// `"...".tt` site is already a Text; `.tt` becomes the identity on it. The
// target name differs because Text erases to String, which would otherwise
// clash with the String `.tt` above. Remove both when the migration
// completes.
extension (text: Text)
  @scala.annotation.targetName("ttIdentity")
  def tt: Text = text

// The compiler's Literate hook: with this given in scope, a string literal
// whose expected type does not require a String is re-typed as `Text`. The
// literal's singleton is deliberately *not* carried as a `Topic` refinement:
// a refined `Text { type Topic = "x" }` is what type inference sees, and it
// leaks into every `Self`-invariant typeclass lookup (`Text is Cuttable by
// Text{…}`), inferred `val`/`var` types and type-parameter instantiation,
// where `t"x"` had simply been `Text`. Until the compiler widens the
// refinement in inference as it widens singletons, a literal is exactly a
// `Text`.
final class TextLiterate[str <: String & Singleton] extends scala.Literate[str]:
  type Result = Text
  inline def convert(inline value: str): Result = value.asInstanceOf[Result]

// In `literacy` rather than at the package top level: wildcard imports and
// exports exclude givens, so the ambient route is a root import
// (`-Yimports:...,anticipation.literacy`), which does carry them — the
// stand-in for the instance's eventual home in the proscenium prelude.
object literacy:
  given literate: [str <: String & Singleton] => TextLiterate[str] = TextLiterate[str]()

export internal.Text
