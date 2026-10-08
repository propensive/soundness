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
package galilei

import scala.quoted.*

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gigantism.*
import prepositional.*
import serpentine.*
import vacuous.*

object internal:
  // The platform-aware `p"…"` literal macro: it decodes the string as a POSIX path, falling back to
  // a Windows path. It lives here with the OS platform types (`Posix`/`Windows`/`Drive`), whose
  // `Radical` givens it needs at expansion time; the generic compile-time path helpers stay in
  // `serpentine.internal`.
  def path(context: Expr[StringContext]): Macro[Path] =
    import quotes.reflect.*

    val name: String = context.valueOrAbort.parts.head

    // Lifted as `String`s, reconstructing the `Text`s at runtime (`.tt`): `ToExpr[Text]` would
    // lift a reach capability into the generated code.
    def liftText(text: Text): Expr[Text] = '{${Expr(text.s)}.tt}

    // The literal's `Topic` is the tuple of its elements' literal types, leaf first, as `/`
    // builds it: `p"/foo/bar/baz"` is a `Path of ("baz", "bar", "foo")`.
    def topic(descent: scala.Seq[Text]): TypeRepr =
      descent.foldRight(TypeRepr.of[EmptyTuple]): (element, tail) =>
        (ConstantType(StringConstant(element.s)).asType, tail.asType) match
          case ('[element], '[type tail <: Tuple; tail]) => TypeRepr.of[element *: tail]

    safely(name.tt.as[Path on Posix]).let: path =>
      val descent = Lifts.list(List.from(path.descent.map(liftText)))

      topic(path.descent).asType.absolve match
        case '[type topic <: Tuple; topic] =>
          '{Path[Posix, %.type, topic](${Expr(path.root)}, $descent)}

    . or:
        safely(name.tt.as[Path on Windows]).let: path =>
          val descent = Lifts.list(List.from(path.descent.map(liftText)))

          topic(path.descent).asType.absolve match
            case '[type topic <: Tuple; topic] =>
              '{Path[Windows, Drive, topic](${Expr(path.root)}, $descent)}

        . or(halt(66, m"The path ${name} is not a valid Windows or POSIX path"))
