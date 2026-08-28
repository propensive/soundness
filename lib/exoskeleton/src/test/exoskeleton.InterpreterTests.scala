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
package exoskeleton

import soundness.*

// Unit coverage for the two POSIX `Interpreter` givens (#559), pinning them against
// each other: under `posixClusteringInterpreter` a short flag longer than two
// characters expands one flag per character (`-abc` ≡ `-a -b -c`), while under
// `posixInterpreter` the same argument is the flag `-a` with operand `bc`. Queries go
// through the given's own `interpret`/`locate` surface, so the path a real `Cli`
// exercises is the path under test. `locate` distinguishes a present flag with no
// operands (`Nil`) from an absent one (`Unset`).
object InterpreterTests extends Suite(m"Commandline interpreter tests"):
  // Renders a `locate` outcome as `Text` (the union `Optional[List[Text]]` has no
  // `CanEqual` against a plain list): `absent` for an unspecified flag, otherwise the
  // operands space-joined in brackets, so a present flag with no operands reads `[]`.
  private def reading(interpreter: Interpreter, flag: Flag, arguments: Text*): Text =
    val topic = interpreter.interpret(Cli.arguments(arguments))
    interpreter.locate(topic, flag).lay(t"absent"): operands =>
      t"[${operands.map(_()).join(t" ")}]"

  def run(): Unit =
    val clustering = interpreters.posixClusteringInterpreter
    val posix = interpreters.posixInterpreter

    suite(m"POSIX clustering interpreter"):
      test(m"`-abc` sets the flag `-a`"):
        reading(clustering, Flag('a'), t"-abc")
      . assert(_ == t"[]")

      test(m"`-abc` sets the flag `-b`"):
        reading(clustering, Flag('b'), t"-abc")
      . assert(_ == t"[]")

      test(m"`-abc` sets the flag `-c`"):
        reading(clustering, Flag('c'), t"-abc")
      . assert(_ == t"[]")

      test(m"`-abc` does not set a flag for an absent character"):
        reading(clustering, Flag('d'), t"-abc")
      . assert(_ == t"absent")

      test(m"an operand attaches to the last flag of a cluster"):
        reading(clustering, Flag('b'), t"-ab", t"x")
      . assert(_ == t"[x]")

      test(m"an operand does not attach to an earlier flag of a cluster"):
        reading(clustering, Flag('a'), t"-ab", t"x")
      . assert(_ == t"[]")

      test(m"an operand attaches to the last of three clustered flags"):
        reading(clustering, Flag('c'), t"-abc", t"x")
      . assert(_ == t"[x]")

      test(m"a two-character flag is not expanded"):
        reading(clustering, Flag('a'), t"-a")
      . assert(_ == t"[]")

      test(m"a two-character flag still takes its operand"):
        reading(clustering, Flag('a'), t"-a", t"x")
      . assert(_ == t"[x]")

      test(m"a long flag is not treated as a cluster"):
        reading(clustering, Flag("verbose"), t"--verbose")
      . assert(_ == t"[]")

      test(m"a long flag sets no per-character flags"):
        reading(clustering, Flag('v'), t"--verbose")
      . assert(_ == t"absent")

      // The occurrence count needs the parameters map, which the `Interpreter` surface
      // does not expose (`locate` seeks the first match); the given's `Topic` is known to
      // be `Commandline` by construction.
      test(m"`-vvv` records one occurrence of `-v` per character"):
        clustering.interpret(Cli.arguments(scala.List(t"-vvv"))).asInstanceOf[Commandline]
        . parameters.to[List].bind:
            (key, _) => if Flag('v', repeatable = true).matches(key) then List(key()) else Nil
      . assert(_ == List(t"-v", t"-v", t"-v"))

    // The same inputs under the non-clustering interpreter take the classic
    // `-ovalue` reading, so neither given can silently acquire the other's
    // behaviour.
    suite(m"POSIX interpreter"):
      test(m"`-abc` is the flag `-a` with operand `bc`"):
        reading(posix, Flag('a'), t"-abc")
      . assert(_ == t"[bc]")

      test(m"`-abc` does not set the flag `-b`"):
        reading(posix, Flag('b'), t"-abc")
      . assert(_ == t"absent")

      test(m"a following argument joins the embedded operand"):
        reading(posix, Flag('a'), t"-abc", t"x")
      . assert(_ == t"[bc x]")

      test(m"a two-character flag takes its operand as under clustering"):
        reading(posix, Flag('a'), t"-a", t"x")
      . assert(_ == t"[x]")

      test(m"a long flag reads identically under both interpreters"):
        reading(posix, Flag("verbose"), t"--verbose")
      . assert(_ == t"[]")
